{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses #-}
module DistanceExtension where

import Util (
    try_parens,
    UniqueNames,
    go,
    ShowParens,
    show_parens,
    Substitute,
    substitute
    )

-- 3.1
data Term a
    = Value Int
    | Add a a
    | Sub a a
    | IfZero a a a

get_value = \case
    Value distance -> Just distance
    _ -> Nothing

instance ShowParens a => ShowParens (Term a) where
    show_parens should = \case
        Value distance ->
            show distance
        Add a b ->
            try_parens should [show_parens True a, " + ", show_parens True b]
        Sub a b ->
            try_parens should [show_parens True a, " - ", show_parens True b]
        IfZero x yes no ->
			try_parens should [
                show_parens True x,
                " z? ",
                show_parens True yes,
                " : ",
                show_parens True no
            ] 

instance UniqueNames a => UniqueNames (Term a) where
    go bound = \case
        Value distance -> Value distance
        Add a b -> Add (go bound a) (go bound b)
        Sub a b -> Sub (go bound a) (go bound b)
        IfZero x yes no -> IfZero (go bound x) (go bound yes) (go bound no)

instance Substitute t a => Substitute t (Term a) where
    substitute substituted_name term = \case
        Value distance -> Value distance
        Add a b -> Add (go a) (go b)
        Sub a b -> Sub (go a) (go b)
        IfZero x yes no -> IfZero (go x) (go yes) (go no)
        where go = substitute substituted_name term