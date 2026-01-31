module Y2024.DAYS.Day2 (day2) where

parse :: String -> [[Int]]
parse s = let readLine = (map read) . words in 
  map readLine $ lines s

gaps :: [Int] -> Bool
gaps level = all (\x -> (abs x) >= 1 && (abs x) <= 3) $ diffs level

strictlyIncreasing :: [Int] -> Bool
strictlyDecreasing level = all (<0) $ diffs level

strictlyDecreasing :: [Int] -> Bool
strictlyIncreasing level = all (>0) $ diffs level

diffs :: [Int] -> [Int]
diffs level = map (\x -> fst x - snd x) $ zip (init level) (drop 1 level)

part1 :: [[Int]] -> Int
part1 levels = sum $ map (fromEnum . isSafe) levels where 
  isSafe level = (gaps level) && (strictlyIncreasing level || strictlyDecreasing level) 


part2 :: [[Int]] -> Int
part2 levels = sum $ map (fromEnum . isSafe) levels where
  isSafe :: [Int] -> Bool
  isSafe level = isSafe' level || (any id $ map isSafe' $ sublevels level)
  
  isSafe' sublevel = (gaps sublevel) && (strictlyIncreasing sublevel || strictlyDecreasing sublevel) 
  
  sublevels :: [Int] -> [[Int]]
  sublevels level = map (getSublevel level)  [0..(length level - 1)]

  getSublevel :: [Int] -> Int -> [Int]
  getSublevel level i = map fst (filter (\x -> snd x /= i) $ zip level [0..(length level - 1)])

day2 :: IO (Int, Int)
day2 = do
  content <- readFile "src/y2024/inputs/day2.txt"
  parsed <- pure $ parse content
  pure $ (part1 parsed, part2 parsed)