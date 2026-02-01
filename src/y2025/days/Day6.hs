module Y2025.DAYS.Day6 (day6) where
import Text.Megaparsec (some, choice)
import Text.Megaparsec.Char (char, eol, hspace, digitChar)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')
import Data.List (transpose)
import Data.Char (isSpace)

-- this was a lot harder to make than you might think. Parsing this thing was a nightmare
pInput :: Parser ([[Int]], String)
pInput = (,) <$> (some (pOperand <* eol)) <*> pOperators
  where
    pOperand = hspace *> some (decimal <* hspace)
    pOperators = hspace *> some (choice [char '*', char '+'] <* hspace)

-- not as bad the second time around
pInput2 :: Parser ([[Char]], String)
pInput2 = (,) <$> (some (pOperand <* eol)) <*> pOperators
  where
    pOperand = some $ choice [digitChar, char ' ']
    pOperators = hspace *> some (choice [char '*', char '+'] <* hspace)

performOp :: ([Int], Char) -> Int
performOp (xs, op) = if op == '*' then product xs else sum xs

part1 :: [[Int]] -> String -> Int
part1 arr ops = sum $ map performOp $ zip (transpose arr) ops

part2 :: [[Char]] -> String -> Int
part2 arr ops = sum $ map performOp $ zip chunked ops
  where
    tArr = transpose arr
    chunked = chunk tArr
    chunk :: [String] -> [[Int]]
    chunk [] = []
    chunk xs = 
      let n = scanUntilSpace xs 
      in (map read $ take n xs) : chunk (drop (n+1) xs)

    scanUntilSpace :: [String] -> Int
    scanUntilSpace (x:xs) = if all isSpace x then 0 else 1 + scanUntilSpace xs
    scanUntilSpace [] = 0

day6 :: IO (Int, Int)
day6 = do
  content <- readFile "src/y2025/inputs/day6.txt"
  let p1 = parse' content $ (uncurry part1) <$> pInput
  let p2 = parse' content $ (uncurry part2) <$> pInput2
  pure (p1,p2)