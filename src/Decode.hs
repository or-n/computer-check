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
        process <- parens term <|> refer
        _ <- space
        input <- parens term <|> refer
        return (Supply process input)
    assume = do
        n <- name
        _ <- string ": "
        usage <- term
        return (Assume n usage)
    term = try assume <|> try supply <|> extensions <|> atom
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
            a <- parens term
            _ <- string " + "
            b <- parens term
            return ( (DistanceExtension.Add a b))
        sub = do
            a <- parens term
            _ <- string " - "
            b <- parens term
            return ( (DistanceExtension.Sub a b))
        if_zero = do
            x <- parens term
            _ <- string " 0? "
            yes <- parens term
            _ <- string " : "
            no <- parens term
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
        try push <|> try top <|> try rest <|> try if_empty
        ) where
        push = do
            top' <- try atom <|> parens term
            _ <- string ", "
            rest' <- try atom <|> parens term
            return (ListExtension.Push top' rest')
        top = do
            pair <- parens term
            _ <- string ".top"
            return (ListExtension.Top pair)
        rest = do
            pair <- parens term
            _ <- string ".rest"
            return (ListExtension.Rest pair)
        if_empty = do
            x <- parens term
            _ <- string " ()? "
            yes <- parens term
            _ <- string " : "
            no <- parens term
            return (ListExtension.IfEmpty x yes no)