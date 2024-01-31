{-# LANGUAGE LambdaCase #-}
module Util where

parens :: Bool -> String -> String
parens in_parens x = if in_parens then concat ["(", x, ")"] else x

find_first :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
find_first f = go [] where
    go rev_before = \case
        (x : after) -> case f x of
            Just y -> Just (y, rev_before ++ after)
            _ -> go (x : rev_before) after
        _ -> Nothing