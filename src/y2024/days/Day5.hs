module Y2024.DAYS.Day5 (day5) where 
import Data.IntMap.Strict(fromListWith, IntMap, lookup)
import Data.List.Split (splitOn)
import Prelude hiding (lookup)
import Data.List (sortBy)

parse :: [String] -> (IntMap [Int], [[Int]])
parse x = 
  let (rules, input) = foldl' parse' ([],[]) x 
  in (fromListWith (++) rules, input) where
    parse' :: ([(Int, [Int])], [[Int]]) -> String -> ([(Int, [Int])], [[Int]])
    parse' (h,t) [a,b,'|',c,d] = ((read [a,b], [read [c,d]]) : h, t)
    parse' (h,t) [a,b,'|',c,d,'\r'] = ((read [a,b], [read [c,d]]) : h, t)
    parse' (h,t) "" = (h,t)
    parse' (h,t) "\r" = (h,t)
    parse' (h,t) rule = (h, (map read $ splitOn "," rule):t)

legal :: IntMap [Int] -> Int -> Int -> Bool -- answers the question: can "a" go before "b" according to a rule set?
legal rules a b = case lookup a rules of
  Just aRules -> not $ elem b aRules
  Nothing -> True 

middle :: [Int] -> Int
middle input = input !! (length input `div` 2)

validInput :: IntMap [Int] -> [Int] -> Bool
validInput rules input = foldl' (&&) True 
  [legal rules a b | (a, i) <- zip input [0::Int ..], (b, j) <- zip input [0..], i > j] 

part1 :: (IntMap [Int], [[Int]]) -> Int
part1 (rules, inputs) = sum $ map middle $ filter (validInput rules) inputs 

part2 :: (IntMap [Int], [[Int]]) -> Int
part2 (rules, inputs) = sum $ map (middle . sortBy comp) badInputs where
  comp :: Int -> Int -> Ordering
  comp a b = compare (legal rules a b) (not $ legal rules a b)
  badInputs :: [[Int]]
  badInputs = filter (not . validInput rules) inputs 

-- expected output: (7365,5770)
day5 :: IO (Int, Int)
day5 = do
    content <- readFile "src/y2024/inputs/day5.txt"
    let parsed = parse $ lines content
    pure $ (part1 parsed, part2 parsed)
