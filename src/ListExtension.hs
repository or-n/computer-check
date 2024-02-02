{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses #-}
module ListExtension where

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
    = End
    | Push a a
    | Top a
    | Rest a
    | IfEmpty a a a

get_push = \case
    Push top rest -> Just (top, rest)
    _ -> Nothing

instance ShowParens a => ShowParens (Term a) where
    show_parens should = \case
        End ->
            "()"
        Push top rest ->
            try_parens True [show_parens False top, ", ", show_parens True rest]
        Top pair ->
            try_parens should [show_parens True pair, ".top"]
        Rest pair ->
            try_parens should [show_parens True pair, ".rest"]
        IfEmpty x yes no ->
			try_parens should [
                show_parens True x,
                " e? ",
                show_parens True yes,
                " : ",
                show_parens True no
            ]

instance UniqueNames a => UniqueNames (Term a) where
    go bound = \case
        End -> End
        Push top rest -> Push (go bound top) (go bound rest)
        Top pair -> Top (go bound pair)
        Rest pair -> Rest (go bound pair)
        IfEmpty x yes no -> IfEmpty (go bound x) (go bound yes) (go bound no)

instance Substitute t a => Substitute t (Term a) where
    substitute substituted_name term = \case
        End -> End
        Push top rest -> Push (go top) (go rest)
        Top pair -> Top (go pair)
        Rest pair -> Rest (go pair)
        IfEmpty x yes no -> IfEmpty (go x) (go yes) (go no)
        where go = substitute substituted_name term 