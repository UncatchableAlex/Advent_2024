module DAYS.Day1 (day1,pIntPair) where

import Data.List (sort)
import UTIL.Parsers (lInt, parse', Parser)
import Text.Megaparsec (many)

-- lex a pair of integers
pIntPair :: Parser (Int, Int)
pIntPair = (,) <$> lInt <*> lInt

part1 :: ([Int], [Int]) -> Int
part1 ls = 
  let sorted = zip (sort $ fst ls) (sort $ snd ls) -- sort the left and right lists and zip them
  in sum $ map (\(a,b) -> abs $ a - b) sorted

part2 :: ([Int], [Int]) -> Int
part2 ls = 
  -- get how many occurrences of a given element are in the right list
  let occs e = length $ (filter (== e) $ snd ls) 
  in sum $ (map (\x -> x * occs x) $ fst ls)

day1 :: IO (Either String (Int, Int))
day1 = do
  content <- readFile "src/inputs/day1.txt"
  case parse' (many pIntPair) content of 
    Left err -> pure $ Left err
    Right listOfTuples -> 
      let tupleOfLists = (map fst listOfTuples, map snd listOfTuples)
      in pure $ Right (part1 tupleOfLists, part2 tupleOfLists)