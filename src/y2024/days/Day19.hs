module Y2024.DAYS.Day19 (day19) where

import Data.Array ((!), (//))
import qualified Data.Array as A
import Text.Megaparsec (sepBy, some)
import Text.Megaparsec.Char (eol, letterChar, string)
import UTIL.Parsers (Parser, parse')

type Towel = String

type Combo = String

pInput :: Parser ([Towel], [Combo])
pInput =
  (,)
    <$> ((sepBy (some letterChar) (string ", ")) <* (some eol))
    <*> (sepBy (some letterChar) eol)

countOrderings :: ([Towel], [Combo]) -> (Int, Int)
countOrderings (towels, combos) =
  let orderings = map part1' combos
   in (length $ filter (> 0) orderings, sum $ orderings)
  where
    initialArr combo = A.array (0, length combo) $ (0, 1) : [(i, 0) | i <- [1 .. length combo]]
    part1' combo = let n = length combo in (! n) $ foldl' (step combo) (initialArr combo) [1 .. n]
    step combo arr i =
      -- how many instances of the subproblem ending at position i (exclusive) contain this towel as the last
      -- towel in the sequence?
      let instances towel =
            if (i >= length towel) && (towel == (drop (i - length towel) $ take i combo))
              then (arr ! (i - (length towel)))
              else 0
          -- how many permissible instances of the subproblem ending at position i (exclusive) exist?
          subproblems = sum $ map instances towels
       in arr // [(i, (arr ! i) + subproblems)]

day19 :: IO (Int, Int)
day19 = do
  input <- readFile "src/y2024/inputs/day19.txt"
  pure $ parse' input $ countOrderings <$> pInput