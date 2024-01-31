{-# LANGUAGE LambdaCase #-}
module TermType where

import Util (parens)

data TermType
    = Generic String
    | Arrow TermType TermType
    | Distance
    deriving Eq

instance Show TermType where
    show = go True where
        go in_parens = \case
            Generic name ->
                name
            Arrow from to ->
                parens in_parens $ concat [go True from, " -> ", go False to]
            Distance ->
                "Distance"

substitute :: String -> TermType -> TermType -> TermType
substitute substituted_name substituted_type = go where
    go = \case
        Generic name | name == substituted_name ->
            substituted_type
        Arrow from to ->
            Arrow (go from) (go to)
        x -> x

get_generic :: TermType -> Maybe String
get_generic = \case
    Generic name -> Just name
    _ -> Nothing

get_arrow :: TermType -> Maybe (TermType, TermType)
get_arrow = \case
    Arrow from to -> Just (from, to)
    _ -> Nothing