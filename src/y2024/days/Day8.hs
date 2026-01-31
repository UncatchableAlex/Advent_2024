module Y2024.DAYS.Day8 (day8) where 
  
import Data.List.Split(splitOn)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (lookup) -- so that we don't accidentally use the wrong lookup
import Control.Monad (guard)

parse :: String -> (Map Char [(Int, Int)], Int)
parse s = (M.fromListWith (++) nodeList, length lined) where
  lined = splitOn "\n" $ filter (/= '\r') s
  nodeList = [(n, [(i,j)]) | (i,row) <- zip [0..] lined, (j,n) <- zip [0..] row, n /= '.']
 
compute :: Int -> ((Int, Int) -> (Int, Int) -> [(Int, Int)]) -> Char -> [(Int, Int)] -> Set (Int, Int)
compute n transform _ nodeLocs = S.fromList $ concat $ do
      r <- nodeLocs
      q <- nodeLocs
      guard $ q /= r
      pure $ filter (\(x,y) -> x < n && y < n && x >= 0 && y >= 0) $ transform r q

part1 :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
part1 (a,b) (c,d) = [((c - a) + c, (d - b) + d)]

part2 :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
part2 n (a,b) (c,d) = do
  let g = gcd (c - a) (d - b)
  r <- [-n..n]
  pure $ (r * ((c - a) `div` g) + a, r * ((d - b) `div` g) + b)

-- expected output: (214,809)
day8 :: IO (Int, Int)
day8 = do
  s <- readFile "src/y2024/inputs/day8.txt"
  let (nodeMap, n) = parse s 
  let p1 = S.size $ M.foldMapWithKey (compute n part1) nodeMap -- we exploit the fact that set is a monoid
  let p2 = S.size $ M.foldMapWithKey (compute n $ part2 n) nodeMap
  pure $ (p1, p2)
