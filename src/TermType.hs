{-# LANGUAGE LambdaCase #-}
module TermType where

import Util (try_parens)

-- 2.3.1
data TermType
    = Generic String
    | Arrow TermType TermType
    | Distance
    | List TermType
    | ForAll String TermType
    | Region TermType
    deriving (Eq, Ord)

get_generic :: TermType -> Maybe String
get_generic = \case
    Generic name -> Just name
    _ -> Nothing

get_arrow :: TermType -> Maybe (TermType, TermType)
get_arrow = \case
    Arrow from to -> Just (from, to)
    _ -> Nothing

get_list :: TermType -> Maybe TermType
get_list = \case
    List item -> Just item
    _ -> Nothing

get_forall :: TermType -> Maybe (String, TermType)
get_forall = \case
    ForAll name usage -> Just (name, usage)
    _ -> Nothing

-- 2.3.2
instance Show TermType where
    show = go True where
        go should = \case
            Generic name ->
                name
            Arrow from to ->
                try_parens should [go True from, " -> ", go False to]
            Distance ->
                "Distance"
            List item ->
                concat ["[", go False item, "]"]
            ForAll name usage ->
                try_parens should ["forall ", name, ": ", go False usage]
            Region item ->
                try_parens should ["Region ", go False item]

-- 2.5.2
substitute :: String -> TermType -> TermType -> TermType
substitute substituted_name substituted_type = go where
    go = \case
        Generic name | name == substituted_name ->
            substituted_type
        Arrow from to ->
            Arrow (go from) (go to)
        List item ->
            List (go item)
        ForAll name usage | name /= substituted_name ->
            ForAll name (go usage)
        x -> x