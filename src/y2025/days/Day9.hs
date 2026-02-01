module Y2025.DAYS.Day9 (day9) where

import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')
import UTIL.Util (nofail)



day9 :: IO (Int, Int)
day9 = do
  content <- readFile "src/y2025/inputs/day9.txt"
  --pure $ parse' content $ connect 1000 3 <$> pInput
  pure (0,0)