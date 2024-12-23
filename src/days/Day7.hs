module DAYS.Day7 (day7) where

import Data.Int (Int64)
import Data.List.Split (splitOn)

parseLine :: String -> (Int64, [Int64])
parseLine s = case splitOn ": " s of
  [x, y] -> (read $ x, map read $ splitOn " " y)
  _ -> error "invalid input"

-- fast function for taking the last N elements of a list. Very clever use of foldl'
-- https://stackoverflow.com/a/17253092
lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)

-- return whether or not a problem is solveable with the allowed operators
compute :: [Int64] -> Int64 -> Bool -> Bool
compute ls target part2 = case (ls, target) of
  ((x : xs), _) -> recurse
    where
      xlen = length $ show x
      tlen = length $ show target
      tTrunc = read $ take (tlen - xlen) (show target)
      recurse
        | x == target = True -- base case
        -- if the target is divisable by x, then we try try to reduce the problem through division first
        | target `mod` x == 0 =
            (compute xs (target `div` x) part2)
              || (part2 && lastN' xlen (show target) == (show x) && compute xs tTrunc part2)
              || compute xs (target - x) part2
        -- if x shares target's last few characters, then we first try to reduce the problem through truncation
        | part2 && lastN' xlen (show target) == (show x) =
            compute xs tTrunc part2
              || compute xs (target - x) part2
        -- if all else fails, then we try subtraction
        | target < x = False
        | otherwise = compute xs (target - x) part2
  ([], 0) -> True
  ([], _) -> False

day7 :: IO (Int, Int)
day7 = do
  s <- readFile "src/inputs/day7.txt"
  let parsed = (map parseLine $ lines s)
  let bools is2 = map (\(target, ls) -> compute (reverse ls) target is2) parsed
  -- transform the boolean list of solveable problems into a useful result:
  let part is2 = sum $ map (fst . snd) $ filter fst $ zip (bools is2) parsed
  pure $ (fromIntegral $ part False, fromIntegral $ part True)
