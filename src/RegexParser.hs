module RegexParser (parseRegex) where

import Text.ParserCombinators.Parsec
import Brzozowski (Regex(..))

regex :: Parser Regex
regex = do
    r <- concatenation
    (Or r <$> (char '+' *> concatenation)) <|> (And r <$> (char '&' *> concatenation)) <|> return r

atom :: Parser Regex
atom = do
    r <- universalSet <|> symbol <|> parentheses regex
    (return (Not r) <* char '\'') <|> (return (Iterate r) <* char '*') <|> return r

concatenation :: Parser Regex
concatenation = concatAtoms <$> many1 atom

concatAtoms :: [Regex] -> Regex
concatAtoms [] = Lambda
concatAtoms [a] = a
concatAtoms (a:as) = Concatenation a $ concatAtoms as

and :: Parser Regex
and = And <$> regex <*> (char '&' *> regex)

or :: Parser Regex
or = Or <$> regex <*> (char '+' *> regex)

not :: Parser Regex
not = Not <$> regex <* char '\''

parentheses :: Parser Regex -> Parser Regex
parentheses p = char '(' *> p <* char ')'

alphabet :: [Char]
alphabet = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

symbol :: Parser Regex
symbol = Symbol <$> oneOf alphabet

universalSet :: Parser Regex
universalSet = const (Not Phi) <$> char 'I'

parseRegex :: String -> Regex
parseRegex p = case runParser (regex <* eof) () "" p of
    Left err -> error ("invalid regex: " ++ show err)
    Right r -> r
