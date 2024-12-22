module DAYS.Day11 (day11) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

update :: (Int, Int) -> [(Int, Int)]
update (k, v)
  | k == 0 = [(1, v)]
  | keyLen `mod` 2 == (0 :: Int) =
      let pow = (10 ^ (keyLen `div` 2))
          prefix = k `div` pow
          suffix = k - (prefix * pow)
       in [(prefix, v), (suffix, v)]
  | otherwise = [(k * 2024, v)]
  where
    keyLen = 1 + floor ((logBase 10 (fromIntegral k)) :: Float)

blink :: IntMap Int -> Int -> IntMap Int
blink stones n = (iterate step stones) !! n
  where
    step stones' = M.fromListWith (+) $ concat $ map update $ M.assocs stones'

-- expected: (220722, 261952051690787)
day11 :: IO (Int, Int)
day11 = do
  input <- readFile "src/inputs/day11.txt"
  let initialStones = M.fromListWith (+) $ zip (map read $ words input) (repeat 1)
  let blink25 = blink initialStones 25
  pure $ (M.foldl' (+) 0 $ blink25, M.foldl' (+) 0 $ blink blink25 50)
