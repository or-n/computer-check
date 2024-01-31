module Decode (decode) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Term

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

name :: Parser String
name = do
    first <- letter
    rest <- many (letter <|> digit <|> char '_' <|> char '\'')
    return (first : rest)

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
    term = try assume <|> try supply <|> refer
