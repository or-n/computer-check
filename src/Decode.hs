module Decode (decode) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Term
import qualified DistanceExtension
import qualified ListExtension

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

name :: Parser String
name = do
    first <- letter
    rest <- many (letter <|> digit <|> char '_' <|> char '\'')
    return (first : rest)

-- 2.1.3
decode :: String -> Either ParseError Term
decode = parse term "" where
    refer = Refer <$> name
    supply = do
        process <- parens term <|> atom
        _ <- space
        input <- parens term <|> atom
        return (Supply process input)
    assume = do
        n <- name
        _ <- string ": "
        usage <- term
        return (Assume n usage)
    term = do
        x <- try assume <|> extensions <|> try supply <|> atom
        suffix x
    define = do
        _ <- char '@'
        n <- name
        _ <- string " := "
        definition <- parens term
        _ <- string " in "
        usage <- term
        return (Define n definition usage)
    extensions = distance_extension <|> list_extension <|> define
    distance_extension = DistanceExtension <$> (
        try add <|> try sub <|> try if_zero
        ) where
        add = do
            a <- parens term <|> atom
            _ <- string " + "
            b <- parens term <|> atom
            return ( (DistanceExtension.Add a b))
        sub = do
            a <- parens term <|> atom
            _ <- string " - "
            b <- parens term <|> atom
            return ( (DistanceExtension.Sub a b))
        if_zero = do
            x <- parens term <|> atom
            _ <- string " z? "
            yes <- parens term <|> atom
            _ <- string " : "
            no <- parens term <|> atom
            return (DistanceExtension.IfZero x yes no)
    distance_value :: Parser Term
    distance_value = do
        digits <- many1 digit
        return (DistanceExtension (DistanceExtension.Value (read digits)))
    end :: Parser Term
    end = do
        _ <- string "()"
        return (ListExtension ListExtension.End)
    atom = distance_value <|> end <|> refer
    list_extension = ListExtension <$> (
        try push <|> try if_empty
        ) where
        push = do
            top' <- try atom <|> parens term
            _ <- string ", "
            rest' <- try atom <|> parens term
            return (ListExtension.Push top' rest')
        if_empty = do
            x <- parens term
            _ <- string " ()? "
            yes <- parens term
            _ <- string " : "
            no <- parens term
            return (ListExtension.IfEmpty x yes no)
    suffix :: Term -> Parser Term
    suffix x =
        try (top x) <|> try (rest x) <|> return x
    top :: Term -> Parser Term
    top x = do
        _ <- string ".top"
        return (ListExtension (ListExtension.Top x))
    rest :: Term -> Parser Term
    rest x = do
        _ <- string ".rest"
        return (ListExtension (ListExtension.Rest x))