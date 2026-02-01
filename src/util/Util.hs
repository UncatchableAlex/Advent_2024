module UTIL.Util (nofail, forceRight, arrayify2d, pairAdd, get2dArrayNeighbors, count, 
                  binarySearch, extractPos2d, euclDist3) where

import Data.Array (Array)
import qualified Data.Array as A

nofail :: Maybe a -> a
nofail a = case a of
  Just a' -> a'
  Nothing -> error "fail detected in nofail"

forceRight :: (Show b) => Either b a -> a
forceRight a = case a of
  Right a' -> a'
  Left e -> error $ show e

arrayify2d :: [[a]] -> Array (Int, Int) a
arrayify2d arr = A.array ((0, 0), (m, n)) eles
  where
    eles = [((i, j), x) | (i, row) <- zip [0 .. m] arr, (j, x) <- zip [0 .. n] row]
    (m, n) = (length arr - 1, length (arr !! 0) - 1)

get2dArrayNeighbors :: Array (Int, Int) a -> (Int, Int) -> [((Int, Int), a)]
get2dArrayNeighbors arr (x, y) = map (\i -> (i, arr A.! i)) $ filter goodCandidate candidates
  where
    ((xmin, ymin), (xmax, ymax)) = A.bounds arr
    candidates = [(x + 1, y), (x - 1, y), (x + 1, y - 1), (x + 1, y + 1), 
                  (x - 1, y - 1), (x - 1, y + 1), (x, y - 1), (x, y + 1)]
    goodCandidate (x', y') = x' >= xmin && x' <= xmax && y' >= ymin && y' <= ymax

pairAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
pairAdd (a, b) (c, d) = (a + c, b + d)

count :: Eq a => (a -> Bool) -> [a] -> Int
count f = length . filter f



binarySearch :: Array Int Int -> Int -> Int
binarySearch arr x = uncurry helper $ A.bounds arr
  where
    helper :: Int -> Int -> Int
    helper lo hi
      | lo >= hi = lo
      | x' < x = helper (mid + 1) hi
      | otherwise = helper lo mid
        where  
          mid = lo + ((hi - lo) `div` 2) 
          x' = arr A.! mid


-- get a list of coordinates correspondong to the positions of a symbol in a 2d list
extractPos2d :: (Eq a) => a -> [[a]] -> [(Int, Int)]
extractPos2d x (n : ns) = [(i, j) | i <- [0 .. length ns], j <- [0 .. length n - 1], (n : ns) !! i !! j == x]
extractPos2d _ [] = []


euclDist3 :: (Floating a) => (a, a, a) -> (a, a, a) -> a
euclDist3 (a1, b1, c1) (a2, b2, c2) = sqrt $ (a1 - a2) ** 2 + (b1 - b2) ** 2 + (c1 - c2) ** 2