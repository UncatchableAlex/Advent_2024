module UTIL.Parsers (Parser, lexeme, lInt, consumeNewline, parse') where

import Data.Void (Void)
import Data.Char (
  --isAlphaNum, 
  isDigit)
import Text.Megaparsec
  ( Parsec,
   -- notFollowedBy,
    satisfy,
    some,
    parse,
    errorBundlePretty, 
    eof
  )
import Text.Megaparsec.Char (space, newline)
  
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

lInt :: Parser Int
lInt = lexeme $ read <$> some (satisfy isDigit)

consumeNewline :: Parser a -> Parser a
consumeNewline p = p <* newline 

parse' :: Parser a -> String -> Either String a
parse' p input = case parse (space *> p <* eof) "" input of 
  Left err -> Left $ errorBundlePretty err
  Right res -> Right $ res
