{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses #-}
module RegionExtension where

import Util (
    UniqueNames,
    go,
    ShowParens,
    show_parens,
    Substitute,
    substitute
    )

data Term a
    = Region a
    | Dereference a
    | Assign a a

instance ShowParens a => ShowParens (Term a) where
    show_parens should = \case
        Region value ->
            concat ["&", show_parens should value]
        Dereference region ->
            concat ["*", show_parens should region]
        Assign region value ->
            concat [show_parens True region, " <- ", show_parens True value]

instance UniqueNames a => UniqueNames (Term a) where
    go bound = \case
        Region value -> Region (go bound value)
        Dereference region -> Dereference (go bound region)
        Assign region value -> Assign (go bound region) (go bound value)

instance Substitute t a => Substitute t (Term a) where
    substitute substituted_name term = \case
        Region value -> Region (go value)
        Dereference region -> Dereference (go region)
        Assign region value -> Assign (go region) (go value)
        where go = substitute substituted_name term
