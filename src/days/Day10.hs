module DAYS.Day10 (day10) where 
import Data.Array (Array, (!))
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Map.Strict (Map)
import UTIL.Parsers(parseArrayOfInts)

type Pos = (Int, Int)

bothParts :: Array Pos Int -> (Int, Int)
bothParts topoMap = (p1, p2) where
  p1 = sum $ map M.size bfsResults
  p2 = sum $ map (M.foldl (+) 0) bfsResults
  bfsResults = map (\x -> bfs x x) trailheads
  trailheads = map fst $ filter (\(_, x) -> x == 0) $ A.assocs topoMap
  ((xmin, ymin), (xmax,ymax)) = A.bounds topoMap

  bfs :: Pos -> Pos -> Map Pos Int
  bfs (x', y') (x,y) -- our parameters are the previous position and the current position
    | x < xmin || x > xmax || y < ymin || y > ymax || -- if this position is invalid return the empty map
      ((x,y) /= (x',y') && topoMap!(x,y) - topoMap!(x',y') /= 1) = M.empty
    | topoMap!(x,y) == 9 = M.fromList [((x,y), 1)] -- if we found the peak, put it in the map 
    | otherwise = M.unionsWith (+) $ map (bfs (x,y)) [(x+1,y), (x-1,y), (x,y+1), (x, y-1)]

-- expected output (617, 1477)
day10 :: IO (Int, Int)
day10 = do
  input <- readFile "src/inputs/day10.txt"
  pure $ bothParts $ parseArrayOfInts input