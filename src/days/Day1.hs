module DAYS.Day1 (day1) where

import Data.List (sort)

-- get the left and right lists from the input
parseInput :: String -> ([Int], [Int])
parseInput input = (map fst listOfTuples, map snd listOfTuples)
  where
    parseLine [x, y] = (read x, read y)
    parseLine _ = error "bad input"
    listOfTuples =  map (parseLine . words) $ lines input

part1 :: ([Int], [Int]) -> Int
part1 ls = 
  let sorted = zip (sort $ fst ls) (sort $ snd ls) -- sort the left and right lists and zip them
  in sum $ map (\(a,b) -> abs $ a - b) sorted

part2 :: ([Int], [Int]) -> Int
part2 ls = 
  -- get how many occurrences of a given element are in the right list
  let occs e = length $ (filter (== e) $ snd ls) 
  in sum $ (map (\x -> x * occs x) $ fst ls)

day1 :: IO (Int, Int)
day1 = do
  content <- readFile "src/inputs/day1.txt"
  let parsedInput = parseInput content
  pure $ (part1 parsedInput, part2 parsedInput)