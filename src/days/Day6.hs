module DAYS.Day6 (day6) where
import Data.List(find)
import Data.Array (array, (!), Array, bounds, (//), listArray, elems, assocs)
import Data.List.Split (splitOn)
import qualified Data.IntMap.Strict as M

-- parse the input
parse :: [String] -> (Array (Int, Int) Char, (Int, Int), Int)
parse (s:ss) =(arr, start, dir) where 
  dir :: Int
  dir = case lookup (arr!start) $ zip (elems syms) [0..] of
    Just d -> d
    Nothing -> error "we found an invalid starting position somehow"

  start :: (Int, Int)
  start =  case find (\(_, sym) -> sym `elem` (elems syms)) $ assocs arr of
    Just (pos, _) -> pos
    Nothing -> error "no starting position!"

  arr = array bnds eles
  bnds = ((0, 0), (length ss, (length s) - 1))
  eles = [((i,j), n) | (i,row) <- zip [0..] (s:ss), (j,n) <- zip [0..] row]

parse _ = error "input not big enough!"

dirs :: Array Int (Int, Int)
dirs = listArray (0, 3) [(-1, 0), (0, 1), (1, 0), (0, -1)]

syms :: Array Int Char
syms = listArray (0, 3) ['^', '>', 'v', '<']

-- check to see if a given position is out of bounds for our array
outOfBounds :: Array (Int, Int) Char -> (Int, Int) -> Bool
outOfBounds arr idx = (fst idx) < (fst $ fst $ bounds arr) 
  || (fst idx) > (fst $ snd $ bounds arr) 
  || (snd idx) < (snd $ fst $ bounds arr) 
  || (snd idx) > (snd $ snd $ bounds arr) 

detectCycle :: [(Int,Int)] -> M.IntMap [Int] -> M.IntMap[Int] -> (Int, Int) -> Int -> Bool
detectCycle visited rowKeyed colKeyed (x,y) dir 
  | x' < 0 || x' > almostInf || y' < 0 || y' > almostInf = False
  | (x', y') `elem` visited = True
  | otherwise = detectCycle ((x,y):visited) rowKeyed colKeyed (x', y') (dir + 1)
  where
    (x', y') = case dir `mod` 4 of
      0 -> ((greatestUnder (get y colKeyed) x (-10)) + 1, y)
      1 -> (x, (leastOver (get x rowKeyed) y inf) - 1)
      2 -> ((leastOver (get y colKeyed) x inf) - 1, y)
      _ -> (x, (greatestUnder (get x rowKeyed) y (-10)) + 1)
    
    greatestUnder (n:ns) target best = let best' = max n best in greatestUnder ns target (if best' >= target then best else best')
    greatestUnder [] _ best = best
    leastOver (n:ns) target best = let best' = min n best in leastOver ns target (if best' <= target then best else best')
    leastOver [] _ best = best
    inf = 1000000
    almostInf = inf - 10
  
    get :: Int ->  M.IntMap [Int] -> [Int]
    get k m = case (M.lookup k m) of
      Nothing -> []
      Just v -> v
  
bothParts :: Array (Int, Int) Char -> M.IntMap [Int] -> M.IntMap[Int] -> (Int, Int) -> Int -> (Int, Int)
bothParts arr rowKeyed colKeyed (x,y) dir = case (oob, arr!(x,y), isCycle) of
  -- if we have left the area, then we hit our base case. We do the final count for part 1 (add one for the start position)
  (True, _, _) -> let p1 = 1 + length (filter (=='X') $ elems arr) in (p1, 0)
  -- if we hit a wall, take a step back in the direction we came from and turn
  (_, '#', _) -> bothParts arr rowKeyed colKeyed (x - dx, y - dy) (dir + 1)
  -- if there's a cycle add it to the count and take a step. Importantly, we mark 
  -- the location of the proposed wall, so we don't double count it later
  (_, _, True) -> let (p1, p2) = bothParts arr' rowKeyed colKeyed (x', y') dir in (p1, p2 + 1)
  -- if there is no cycle, just move forward one step
  (_, _, _) -> bothParts arr' rowKeyed colKeyed (x', y') dir
  where
    (dx, dy) = dirs!(dir `mod` 4)
    (x', y') = (x + dx, y + dy)
    arr' = if arr!(x,y) == '.' then arr // [((x,y), 'X')] else arr
    oob = outOfBounds arr (x,y)
    -- This is an interesting edge case. If we ever double back on ourselves, we cannot place a wall there because would have run 
    -- into it getting to our current position. we therefore check that we have NOT visited the proposed wall location
    isCycle = (not $ outOfBounds arr (x', y')) && arr!(x',y') == '.' && detectCycle [] rowKeyed' colKeyed' (x, y) (dir + 1)
    rowKeyed' = M.insertWith (++) x' [y'] rowKeyed
    colKeyed' = M.insertWith (++) y' [x'] colKeyed
    
-- expecting (4647, 1723)
day6 :: IO (Int, Int)
day6 = do
  s <- readFile "src/inputs/day6.txt"
  let (arr, start,dir) = parse $ splitOn "\r\n" s
  let mapByCol = M.fromListWith (++) [(col, [row]) | ((row, col), n) <- assocs arr, n == '#']
  let mapByRow = M.fromListWith (++) [(row, [col]) | ((row, col), n) <- assocs arr, n == '#']
  pure $ bothParts (arr // [(start, 'O')]) mapByRow mapByCol start dir
