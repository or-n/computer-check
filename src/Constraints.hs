{-# LANGUAGE LambdaCase #-}
module Constraints where

import Term
import TermType
import qualified Data.Map as Map
import Data.Tuple (swap)
import Util (find_first)
import Control.Applicative ((<|>))
import qualified DistanceExtension
import qualified ListExtension
import qualified RegionExtension
import Data.List (nub, (\\))

type Pair = (TermType, TermType)

-- 2.4
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
        -- 3.2
        DistanceExtension extension -> case extension of
            DistanceExtension.Value distance ->
                Just ([(target_type, Distance)], env)
            DistanceExtension.Add a b -> do
                (a_equations, env2) <- go env Distance a
                (b_equations, env3) <- go env2 Distance b
                let both = a_equations ++ b_equations
                Just ((target_type, Distance) : both, env3)
            DistanceExtension.Sub a b -> do
                (a_equations, env2) <- go env Distance a
                (b_equations, env3) <- go env2 Distance b
                let both = a_equations ++ b_equations
                Just ((target_type, Distance) : both, env3)
            DistanceExtension.IfZero x yes no -> do
                (x_equations, env2) <- go env Distance x
                (yes_equations, env3) <- go env2 target_type yes
                (no_equations, env4) <- go env3 target_type no
                Just (x_equations ++ yes_equations ++ no_equations, env4)
        ListExtension extension -> case extension of
            ListExtension.End -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                Just ([(target_type, List item_type)], env2)
            ListExtension.Push top rest -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (top_equations, env3) <- go env2 item_type top
                (rest_equations, env4) <- go env3 (List item_type) rest
                let both = top_equations ++ rest_equations
                Just ((target_type, List item_type) : both, env4)
            ListExtension.Top pair -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (equations, env3) <- go env2 (List item_type) pair
                Just ((target_type, item_type) : equations, env3)
            ListExtension.Rest pair -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (equations, env3) <- go env2 (List item_type) pair
                Just ((target_type, List item_type) : equations, env3)
            ListExtension.IfEmpty x yes no -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (x_equations, env3) <- go env2 (List item_type) x
                (yes_equations, env4) <- go env3 target_type yes
                (no_equations, env5) <- go env4 target_type no
                Just (x_equations ++ yes_equations ++ no_equations, env5)
        RegionExtension extension -> case extension of
            RegionExtension.Reference value ->
                Nothing
            RegionExtension.Dereference region ->
                Nothing
            RegionExtension.Assign region value ->
                Nothing
            RegionExtension.Region index ->
                Nothing
        Define name definition usage -> do
            let definition_type = Generic ("let" ++ show (let_count env))
            let env2 = env
                    { types = Map.insert name definition_type (types env)
                    , let_count = let_count env + 1
                    }
            (definition_equations, env3) <- go env2 definition_type definition
            (usage_equations, env4) <- go env3 target_type usage
            Just (definition_equations ++ usage_equations, env4)

-- 3.4.1
generalize term_type = foldr ForAll term_type names where
    go = \case
        Generic name -> [name]
        Arrow from to -> go from ++ go to
        Distance -> []
        List item -> go item 
        ForAll name usage -> nub (go usage) \\ [name]
        Region item -> go item
    names = nub (go term_type)

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

-- 2.5.1
occurs_in :: String -> TermType -> Bool
occurs_in checked_name = go where
    go = \case
        Generic name ->
            name == checked_name
        Arrow from to ->
            go from || go to
        List item ->
            go item
        ForAll name usage | name /= checked_name ->
            go usage
        Region item ->
            go item
        _ ->
            False

-- 2.5.2
substitute :: String -> TermType -> [Pair] -> [Pair]
substitute substituted_name substituted_type =
    map (\(left, right) -> (go left, go right))
    where
    go = TermType.substitute substituted_name substituted_type

-- 2.5.4
resolve :: [Pair] -> IO [Pair]
resolve equations =
    if length equations > 1 then do
        --print equations
        resolve . nub . shrink_modifying_rest . shrink . expand $ equations
    else
        return equations
    where
    expand = branch unification_step_match_list . branch unification_step_match
    shrink = branch unification_step_forall . branch unification_step_same
    shrink_modifying_rest xs = case find_first unification_step_let xs of
        Just (substitution, rest) -> substitution rest
        _ -> xs
    branch = concatMap . \step pair -> case step pair of
        Just new -> new
        _ -> [pair]
    -- 2.5.3
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
    unification_step_forall pair = go pair <|> go (swap pair) where
        go (left, right) = do
            (name, usage) <- get_forall left
            Just [(usage, right)]
    unification_step_match_list (left, right) = do
        left_item <- get_list left
        right_item <- get_list right
        Just [(left_item, right_item)]

-- 2.5.4
infer :: Term -> IO (Maybe TermType)
infer term = case gen_equations empty_env (Generic "target") term of
    Just equations -> do
        result <- resolve (nub equations)
        return $ generalize <$> case result of
            [(Generic _, x)] -> Just x
            [(x, Generic _)] -> Just x
            _ -> Nothing
    _ -> return Nothing