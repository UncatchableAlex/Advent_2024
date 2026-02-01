module Y2025.DAYS.Day3 (day3) where

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char (digitChar, eol)
import UTIL.Parsers (Parser, parse')
import UTIL.Util (nofail)

pInput :: Parser [[Int]]
pInput = sepBy (many (digitToInt <$> digitChar)) eol

joltage :: Int -> [Int] -> Int
joltage n xs =
  if n < 0
    then 0
    else m * (10 ^ n) + joltage (n - 1) (drop (i + 1) xs)
  where
    stub = take (length xs - n) xs
    m = maximum stub
    i = nofail $ elemIndex m stub

day3 :: IO (Int, Int)
day3 = do
  content <- readFile "src/y2025/inputs/day3.txt"
  let run n = parse' content (sum . map (joltage n) <$> pInput)
  pure (run 1, run 11)
