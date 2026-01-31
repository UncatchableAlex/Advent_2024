module Y2024.DAYS.Day9 (day9) where

import Data.List (find, partition)
import Data.List.Split (splitOn)

partial :: Int -> Int -> Int -> Int
partial diskSectionStart diskSectionSize diskSectionIdx =
  sum [diskSectionStart .. diskSectionStart + diskSectionSize - 1] * diskSectionIdx `div` 2

part1 :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int
part1 ((f, a, i) : fs) ((u', b, j) : us') ((u, c, k) : us)
  | j == k = partial a u k
  | f > u = (partial a u k) + part1 ((f - u, a + u, i) : fs) ((u', b, j) : us') us
  | otherwise = (partial a f k) + (partial b u' j) + part1 fs us' ((u - f, c, k) : us)
part1 _ _ _ = error "not enough free space"

part2 :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int
part2 fs ((u, c, k) : us) = case find (\(f, _, i) -> f >= u && i < k) fs of -- find a free spot for the file
  Just (f, a, i) -> case splitAt (i `div` 2) fs of -- modify the free section that we are putting the file into
    (before, (_ : after)) -> partial a u k + part2 (before ++ (f - u, a + u, i) : after) us
    _ -> error "impossible case"
  Nothing -> (partial c u k) + part2 fs us -- if we couldn't find a spot for our file. keep it where it is
part2 _ [] = 0

-- expected output: (6288599492129,6321896265143)
day9 :: IO (Int, Int)
day9 = do
  input <- readFile "src/y2024/inputs/day9.txt"
  let disk = drop 1 $ map read $ splitOn "" input
  let diskSums = scanl1 (+) (0 : disk)
  let isOdd = (\(_, _, i) -> i `mod` 2 /= 0)
  let (odds, evens) = partition isOdd $ zip3 disk diskSums [0 :: Int ..]
  pure $ (part1 odds (drop 1 evens) $ reverse evens, part2 odds $ reverse evens)
