{-# LANGUAGE LambdaCase #-}
module Constraints where

import Term
import TermType
import qualified Data.Map as Map
import Data.Tuple (swap)
import Util (find_first)
import Control.Applicative ((<|>))

type Pair = (TermType, TermType)

gen_equations :: Env -> TermType -> Term -> Maybe [Pair]
gen_equations init_env init_target_type term = fst <$> result where
    result = go init_env init_target_type term
    go env target_type = \case
        Refer name -> do
            found_type <- Map.lookup name (types env)
            Just ([(found_type, target_type)], env)
        Supply process input -> do
            let input_type = Generic ("let" ++ show (let_count env))
            let env2 = env { let_count = let_count env + 1 }
            (process_equations, env3) <- go env2 (Arrow input_type target_type) process
            (input_equations, env4) <- go env3 input_type input
            Just (process_equations ++ input_equations, env4)
        Assume name usage -> do
            let from = Generic ("a" ++ show (from_count env))
            let to = Generic ("r" ++ show (to_count env))
            let env2 = env
                    { types = Map.insert name from (types env)
                    , from_count = from_count env + 1
                    , to_count = to_count env + 1
                    }
            (usage_equations, env3) <- go env2 to usage
            Just ((target_type, Arrow from to) : usage_equations, env3)

data Env = Env
    { types :: Map.Map String TermType
    , from_count :: Int
    , to_count :: Int
    , let_count :: Int
    }

empty_env :: Env
empty_env = Env
    { types = Map.empty
    , from_count = 0
    , to_count = 0
    , let_count = 0
    }

occurs_in :: String -> TermType -> Bool
occurs_in checked_name = go where
    go = \case
        Generic name ->
            name == checked_name
        Arrow from to ->
            go from || go to
        Distance ->
            False

substitute :: String -> TermType -> [Pair] -> [Pair]
substitute substituted_name substituted_type =
    map (\(left, right) -> (go left, go right))
    where
    go = TermType.substitute substituted_name substituted_type

resolve :: [Pair] -> [Pair]
resolve equations =
    if length equations > 1 then
        resolve . shrink_modifying_rest . shrink . expand $ equations
    else
        equations
    where
    expand = branch unification_step_match
    shrink = branch unification_step_same
    shrink_modifying_rest xs = case find_first unification_step_let xs of
        Just (substitution, rest) -> substitution rest
        _ -> xs
    branch = concatMap . \step pair -> case step pair of
        Just new -> new
        _ -> [pair]
    unification_step_same (left, right) =
        if left == right then
            Just []
        else
            Nothing
    unification_step_let pair = go pair <|> go (swap pair) where
        go (left, right) = do
            name <- get_generic left
            if name == "target" || name `occurs_in` right then
                Nothing
            else
                Just (Constraints.substitute name right)
    unification_step_match (left, right) = do
        (from_left, to_left) <- get_arrow left
        (from_right, to_right) <- get_arrow right
        Just [(from_left, from_right), (to_left, to_right)]

infer :: Term -> Maybe TermType
infer term = case resolve <$> gen_equations empty_env (Generic "target") term of
    Just [(Generic _, x)] -> Just x
    Just [(x, Generic _)] -> Just x
    _ -> Nothing