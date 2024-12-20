module UTIL.Parsers (
  Parser,
  lexeme,
  lInt,
  consumeNewline,
  parse',
  parseArrayOfInts
  ) where

import Data.Void (Void)
import Data.Array (Array)
import qualified Data.Array as A
import qualified Data.List.NonEmpty as N
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
import Data.List.Split (splitOn)
  
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

lInt :: Parser Int
lInt = lexeme $ read <$> some (satisfy isDigit)

consumeNewline :: Parser a -> Parser a
consumeNewline p = p <* newline 

parse' :: String -> Parser a -> a
parse' input p = case parse (space *> p <* eof) "" input of 
  Left err -> error $ errorBundlePretty err
  Right res -> res

parseArrayOfInts :: String -> Array (Int, Int) Int
parseArrayOfInts input = A.array bnds eles where
  parsed' = N.fromList $ map N.fromList $ map (splitOn "") $ lines input 
  parsed = N.map (\row -> (map read row) :: [Int]) $ N.map N.tail parsed'
  eles = [((i,j), x) | (row, i) <- zip (N.toList parsed) [0..], (x, j) <- zip row [0..]]
  bnds = ((0,0), (length parsed - 1, (length $ N.head parsed) - 1))
