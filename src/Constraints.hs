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
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

type Pair = (TermType, TermType)

-- 2.4
gen_equations :: Env -> TermType -> Term -> IO (Maybe [Pair])
gen_equations init_env init_target_type term = (fst <$>) <$> result where
    result = runMaybeT $ go init_env init_target_type term
    m = MaybeT . return
    go env target_type = \case
        Refer name -> do
            found_type <- m $ Map.lookup name (types env)
            return ([(found_type, target_type)], env)
        Supply process input -> do
            let input_type = Generic ("let" ++ show (let_count env))
            let env2 = env { let_count = let_count env + 1 }
            (process_equations, env3) <- go env2 (Arrow input_type target_type) process
            (input_equations, env4) <- go env3 input_type input
            return (process_equations ++ input_equations, env4)
        Assume name usage -> do
            let from = Generic ("a" ++ show (from_count env))
            let to = Generic ("r" ++ show (to_count env))
            let env2 = env
                    { types = Map.insert name from (types env)
                    , from_count = from_count env + 1
                    , to_count = to_count env + 1
                    }
            (usage_equations, env3) <- go env2 to usage
            return ((target_type, Arrow from to) : usage_equations, env3)
        -- 3.2
        DistanceExtension extension -> case extension of
            DistanceExtension.Value _ ->
                return ([(target_type, Distance)], env)
            DistanceExtension.Add a b -> do
                (a_equations, env2) <- go env Distance a
                (b_equations, env3) <- go env2 Distance b
                let both = a_equations ++ b_equations
                return ((target_type, Distance) : both, env3)
            DistanceExtension.Sub a b -> do
                (a_equations, env2) <- go env Distance a
                (b_equations, env3) <- go env2 Distance b
                let both = a_equations ++ b_equations
                return ((target_type, Distance) : both, env3)
            DistanceExtension.IfZero x yes no -> do
                (x_equations, env2) <- go env Distance x
                (yes_equations, env3) <- go env2 target_type yes
                (no_equations, env4) <- go env3 target_type no
                return (x_equations ++ yes_equations ++ no_equations, env4)
        ListExtension extension -> case extension of
            ListExtension.End -> do
                let item = "let" ++ show (let_count env)
                let item_type = Generic item
                let env2 = env { let_count = let_count env + 1 }
                return ([(target_type, ForAll item (List item_type))], env2)
            ListExtension.Push top rest -> do
                let item = "let" ++ show (let_count env)
                let item_type = Generic item
                let env2 = env { let_count = let_count env + 1 }
                (top_equations, env3) <- go env2 item_type top
                (rest_equations, env4) <- go env3 (List item_type) rest
                let both = top_equations ++ rest_equations
                return ((target_type, ForAll item (List item_type)) : both, env4)
            ListExtension.Top pair -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (equations, env3) <- go env2 (List item_type) pair
                return ((target_type, item_type) : equations, env3)
            ListExtension.Rest pair -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (equations, env3) <- go env2 (List item_type) pair
                return ((target_type, List item_type) : equations, env3)
            ListExtension.IfEmpty x yes no -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (x_equations, env3) <- go env2 (List item_type) x
                (yes_equations, env4) <- go env3 target_type yes
                (no_equations, env5) <- go env4 target_type no
                return (x_equations ++ yes_equations ++ no_equations, env5)
        RegionExtension extension -> case extension of
            RegionExtension.Reference value -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (value_equations, env3) <- go env2 item_type value
                return ((target_type, Region item_type) : value_equations, env3)
            RegionExtension.Dereference region -> do
                let region_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (region_equations, env3) <- go env2 region_type region
                return ((Region target_type, region_type) : region_equations, env3)
            RegionExtension.Assign region value -> do
                let item_type = Generic ("let" ++ show (let_count env))
                let env2 = env { let_count = let_count env + 1 }
                (region_equations, env3) <- go env2 (Region item_type) region
                (value_equations, env4) <- go env3 item_type value
                let both = region_equations ++ value_equations
                return ((target_type, End) : both, env4)
            RegionExtension.Region _ ->
                return ([], env)
        Define name definition usage -> do
            let target = Generic "target"
            let env2 = env
                    { types = Map.insert name target (types env)
                    , let_count = let_count env + 1
                    }
            (definition_equations, _) <- go env2 target definition
            t <- liftIO $ resolve definition_equations
            let full_t = generalize t
            let env3 = env
                    { types = Map.insert name full_t (types env)
                    , let_count = let_count env + 1
                    }
            (new, env4) <- go env3 full_t definition
            (usage_equations, env5) <- go env4 target_type usage
            return (new ++ usage_equations, env5)

-- 3.4.1
generalize :: TermType -> TermType
generalize term_type = foldr ForAll term_type names where
    go = \case
        Generic name -> [name]
        Arrow from to -> go from ++ go to
        Distance -> []
        List item -> go item 
        ForAll name usage -> nub (go usage) \\ [name]
        Region item -> go item
        End -> []
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
resolve :: [Pair] -> IO TermType
resolve = \case
    [] -> return End
    [(Generic _, x)] -> return x
    [(x, Generic _)] -> return x
    equations -> do
        --print equations
        let f = if length equations > 1 then shrink_modifying_rest else id
        resolve . nub . f . shrink . expand $ equations
    where
    expand = branch match_region . branch match_list . branch match_arrow
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
    match_arrow (left, right) = do
        (from_left, to_left) <- get_arrow left
        (from_right, to_right) <- get_arrow right
        Just [(from_left, from_right), (to_left, to_right)]
    unification_step_forall pair = go pair <|> go (swap pair) where
        go (left, right) = do
            (name, usage) <- get_forall left
            polymorhic_match name usage right
    match_list (left, right) = do
        left_item <- get_list left
        right_item <- get_list right
        Just [(left_item, right_item)]
    match_region (left, right) = do
        left_item <- get_region left
        right_item <- get_region right
        Just [(left_item, right_item)]

polymorhic_match :: String -> TermType -> TermType -> Maybe [Pair]
polymorhic_match polymorphic_name usage other = case (usage, other) of
    (Generic name1, Generic name2) | name1 == name2 ->
        Just []
    (Generic _, _) ->
        Just [(usage, other)]
    (_, Generic _) ->
        Just [(usage, other)]
    (Arrow from1 to1, Arrow from2 to2) -> do
        from_equations <- go from1 from2
        top_equations <- go to1 to2
        Just (from_equations ++ top_equations)
    (Distance, Distance) ->
        Just []
    (List item1, List item2) ->
        go item1 item2
    (ForAll name1 usage1, _) ->
        polymorhic_match name1 usage1 other
    (_, ForAll name2 usage2) ->
        polymorhic_match name2 usage2 usage
    (Region item1, Region item2) ->
        go item1 item2
    (End, End) ->
        Just []
    _ ->
        Nothing
    where
    go = polymorhic_match polymorphic_name

-- 2.5.4
infer :: Term -> IO (Maybe TermType)
infer term = do
    r <- gen_equations empty_env (Generic "target") term
    case r of
        Just equations -> resolve (nub equations) >>= return . Just . generalize
        _ -> return Nothing