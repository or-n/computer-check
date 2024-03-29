{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses #-}
module RegionExtension where

import Util (
    try_parens,
    UniqueNames,
    go,
    ShowParens,
    show_parens,
    Substitute,
    substitute
    )

-- 4.1
data Term a
    = Reference a
    | Dereference a
    | Assign a a
    | Region Int

get_region = \case
    Region index -> Just index
    _ -> Nothing

instance ShowParens a => ShowParens (Term a) where
    show_parens should = \case
        Reference value ->
            try_parens should ["&", show_parens should value]
        Dereference region ->
            try_parens should ["*", show_parens should region]
        Assign region value ->
            try_parens should [show_parens True region, " <- ", show_parens True value]
        Region index ->
            try_parens should ["#", show index]

instance UniqueNames a => UniqueNames (Term a) where
    go bound = \case
        Reference value -> Reference (go bound value)
        Dereference region -> Dereference (go bound region)
        Assign region value -> Assign (go bound region) (go bound value)
        Region index -> Region index

instance Substitute t a => Substitute t (Term a) where
    substitute substituted_name term = \case
        Reference value -> Reference (go value)
        Dereference region -> Dereference (go region)
        Assign region value -> Assign (go region) (go value)
        Region index -> Region index
        where go = substitute substituted_name term
