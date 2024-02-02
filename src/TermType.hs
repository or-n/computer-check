{-# LANGUAGE LambdaCase #-}
module TermType where

import Util (parens)

-- 2.3.1
data TermType
    = Generic String
    | Arrow TermType TermType
    | Distance
    | List TermType
    | ForAll String TermType
    deriving Eq

get_generic :: TermType -> Maybe String
get_generic = \case
    Generic name -> Just name
    _ -> Nothing

get_arrow :: TermType -> Maybe (TermType, TermType)
get_arrow = \case
    Arrow from to -> Just (from, to)
    _ -> Nothing

-- 2.3.2
instance Show TermType where
    show = go True where
        go in_parens = \case
            Generic name ->
                name
            Arrow from to ->
                parens in_parens $ concat [go True from, " -> ", go False to]
            Distance ->
                "Distance"
            List item ->
                concat ["[", go False item, "]"]
            ForAll name usage ->
                parens in_parens $ concat ["forall ", name, ": ", go False usage]

-- 2.5.2
substitute :: String -> TermType -> TermType -> TermType
substitute substituted_name substituted_type = go where
    go = \case
        Generic name | name == substituted_name ->
            substituted_type
        Arrow from to ->
            Arrow (go from) (go to)
        x -> x