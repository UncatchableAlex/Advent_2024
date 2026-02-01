module Y2025.DAYS.Day2 (day2) where

import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')

pInput :: Parser [(Int, Int)]
pInput = sepBy pRange (char ',')
  where
    pRange :: Parser (Int, Int)
    pRange = (,) <$> (decimal <* char '-') <*> decimal

part1 :: (Int, Int) -> Int
part1 (a, b) = sum $ filter repeats [a .. b]
  where
    repeats :: Int -> Bool
    repeats c =
      let strC = show c
          lenC = length strC
       in strC == (concat $ replicate 2 $ take (lenC `div` 2) strC)

part2 :: (Int, Int) -> Int
part2 (a, b) = sum $ filter repeats [a .. b]
  where
    repeats :: Int -> Bool
    repeats c =
      let strC = show c
          lenC = length strC
          assembleRepeating i = concat $ replicate (lenC `div` i) $ take i strC
       in any ((strC ==) . assembleRepeating) [1 .. (lenC `div` 2)]

day2 :: IO (Int, Int)
day2 = do
  content <- readFile "src/y2025/inputs/day2.txt"
  let runPart f = parse' content $ pInput >>= pure . sum . map f
  pure (runPart part1, runPart part2)
