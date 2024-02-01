{-# LANGUAGE LambdaCase #-}
module DistanceExtension where

import Util (try_parens, UniqueNames, go, ShowParens, show_parens)

-- 3.1
data Term a
    = Value Int
    | Add a a
    | Sub a a
    | IfZero a a a

instance ShowParens a => ShowParens (Term a) where
    show_parens should = \case
        Value distance ->
            show distance
        Add a b ->
            try_parens should [show_parens True a, " + ", show_parens True b]
        Sub a b ->
            try_parens should [show_parens True a, " - ", show_parens True b]
        IfZero x yes no ->
			try_parens should [show_parens False x, " 0? ", show_parens False yes, " : ", show_parens False no] 

instance UniqueNames a => UniqueNames (Term a) where
    go bound = \case
        Value distance -> Value distance
        Add a b -> Add (go bound a) (go bound b)
        Sub a b -> Sub (go bound a) (go bound b)
        IfZero x yes no -> IfZero (go bound x) (go bound yes) (go bound no)