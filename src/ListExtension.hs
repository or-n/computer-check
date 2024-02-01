{-# LANGUAGE LambdaCase #-}
module ListExtension where

import Util (try_parens, UniqueNames, go, ShowParens, show_parens)

-- 3.1
data Term a
    = End
    | Push a a
    | Top a
    | Rest a
    | IfEmpty a a a

instance ShowParens a => ShowParens (Term a) where
    show_parens should = \case
        End ->
            "()"
        Push top rest ->
            try_parens should [show_parens False top, ", ", show_parens True rest]
        Top pair ->
            try_parens should [show_parens False pair, ".top"]
        Rest pair ->
            try_parens should [show_parens False pair, ".rest"]
        IfEmpty x yes no ->
			try_parens should [show_parens False x, " ()? ", show_parens False yes, " : ", show_parens False no]

instance UniqueNames a => UniqueNames (Term a) where
    go bound = \case
        End -> End
        Push top rest -> Push (go bound top) (go bound rest)
        Top pair -> Top (go bound pair)
        Rest pair -> Rest (go bound pair)
        IfEmpty x yes no -> IfEmpty (go bound x) (go bound yes) (go bound no)