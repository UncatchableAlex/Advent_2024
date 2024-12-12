module DAYS.Day9 (day9) where 

import Data.List (partition)
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Data.List.NonEmpty ( zip, NonEmpty( (:|) ) )
import Prelude hiding (zip)

data Disk = Disk {
  free :: [(Int, Int)],
  used :: [(Int, Int)],
  freeIdx :: Int,
  partial :: Int
} deriving Show 

finalSum :: Disk -> Int 
finalSum d =  (partial d) + (foldl' (\c (u, i) -> c + u*i) 0 $ used d)

part1 :: NonEmpty Int -> Int
part1 disk = finalSum $ last $ takeWhile keepGoing $ iterate step $ firstDisk
    where
      keepGoing d = case (free d, used d) of
        ((_,i):_, (_,j):_) -> i < j
        _ -> error "no disk space"

      (odds, evens') = partition (\(_, i) -> i `mod` 2 /= 0) $ zip disk (0 :| [1..])
      firstDisk = Disk {freeIdx = fst $ head 1 evens', partial = 0, free = odds, used = reverse evens'}

      step d = trace (show d) $ case (free d, used d) of
        ((f,i):fs, (u,j):us) -> if f > u 
          then d {partial = (j `div` 2)*(sum [i..i+u-1]) + partial d, free = (f-u,i+u):fs, used = us}
          else d {partial = (j `div` 2)*(sum [i..i+f-1]) + partial d, free = fs, used = (u-f,j+f):us}
        _ -> error "no disk space"
      


-- expected output: (214,809)
day9 :: IO (Int, Int)
day9 = do
  d <- readFile "src/inputs/day9.txt"
  let disk = drop 1 $ map read $ splitOn "" d
  print $ part1 disk
  pure (part1 disk,-1)
  