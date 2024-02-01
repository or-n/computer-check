{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}
module Util where

import qualified Data.Map as Map

parens :: Bool -> String -> String
parens in_parens x = if in_parens then concat ["(", x, ")"] else x

find_first :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
find_first f = go [] where
    go rev_before = \case
        (x : after) -> case f x of
            Just y -> Just (y, rev_before ++ after)
            _ -> go (x : rev_before) after
        _ -> Nothing

class ShowParens a where
    show_parens :: Bool -> a -> String

try_parens in_parens = parens in_parens . concat

class UniqueNames a where
	go :: Map.Map String String -> a -> a

class Substitute t a where
    substitute :: String -> t -> a -> a