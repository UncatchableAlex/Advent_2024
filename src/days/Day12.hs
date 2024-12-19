module DAYS.Day12 (day12) where 

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as N
import qualified Data.Set as S
import Data.Set (Set)

type Pos = (Int, Int)

countSides :: Set Pos -> Set Pos -> [Pos] -> Int
countSides region kernels visited = case visited of
  ((x,y):visited') -> corners + countSides region kernels' visited'
    where
      k1 = [(x-1, y-1), (x-1, y), (x, y-1), (x,y)]
      k2 = [(x-1, y), (x-1, y+1), (x, y), (x,y+1)]
      k3 = [(x, y-1), (x, y), (x+1, y-1), (x+1,y)]
      k4 = [(x, y), (x, y+1), (x+1, y), (x+1,y+1)]
      ks = filter (\k -> S.notMember (k !! 0) kernels) [k1,k2,k3,k4]
      kcount k = length $ filter (\pos -> S.notMember pos region) k
      isOpDiag k = ((S.member (k !! 0) region) && (S.member (k !! 3) region)) || 
        ((S.member (k !! 1) region) && (S.member (k !! 2) region))
      normalCorners = filter (\k -> kcount k `mod` 2 == 1) ks
      kernels' = foldl' (\set kernel -> S.insert (kernel !! 0) set) kernels normalCorners
      opDiagCorners = filter (\k -> kcount k == 2 && isOpDiag k) ks
      corners =  (length normalCorners) + (length opDiagCorners)
  [] -> 0

compute :: Map Pos Char -> Pos
compute plots = (p1, p2) where
  (p1, p2, _) = N.head $ N.fromList $ dropWhile keepGoing $ iterate step (0, 0, plots)
  keepGoing (_, _, unvisited) = M.size unvisited > 0

  step :: (Int, Int, Map Pos Char) -> (Int, Int, Map Pos Char)
  step (p1', p2', unvisitedPlots) = 
    let 
      searchStart = (N.head $ N.fromList $ M.keys unvisitedPlots)
      color = (unvisitedPlots M.! searchStart)
      (visited, edges) = bfs S.empty color searchStart
      unvisited' = M.withoutKeys unvisitedPlots visited
      p1'' = (length edges * S.size visited)
      sides = countSides visited S.empty $ S.toList visited
    in
       (p1' + p1'', (sides * S.size visited) + p2', unvisited')

  foldFunction :: Char -> (Set Pos, [Pos]) -> Pos ->  (Set Pos,  [Pos])
  foldFunction color (region, edges') (x,y) = let (region', edges'') = bfs region color (x,y)
    in (S.union region region', edges' ++ edges'')

  outsideRegion color pos = (not $ M.member pos plots) || plots M.! pos /= color

  bfs :: Set Pos -> Char -> Pos -> (Set Pos, [Pos])
  bfs visited color (x,y)
    | outsideRegion color (x,y) = (S.empty, [(x,y)])
    | S.member (x,y) visited = (S.empty, []) 
    | otherwise = 
      let 
        visited' = S.insert (x,y) visited
      in 
        foldl' (foldFunction color) (visited', []) [(x+1,y), (x-1,y), (x, y+1), (x, y-1)]

-- expect (1518548,909564)
day12 :: IO Pos
day12 = do
  input <- readFile "src/inputs/day12.txt"
  let plots = M.fromList $ [((i,j), x) | (row, i) <- zip (lines input) [0..], (x,j) <- zip row [0..]]
  pure $ compute $ plots
