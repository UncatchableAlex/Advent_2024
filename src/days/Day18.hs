module DAYS.Day18 (day18) where 

import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')
import qualified Data.Array as A
import Data.Array (Array)
import UTIL.PQueue (PQueue)
import qualified UTIL.PQueue as PQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromJust)

type RAM = Array (Int, Int) Bool

type Pos = (Int, Int)

height :: Int 
height = 70

width :: Int 
width = 70

pRam :: Int -> Parser RAM
pRam lns = fmap (ram A.//) (corruptMemory <$> (pCorruption >>= (pure . take lns)))
  where
    ram = A.array ((0,0), (height, width)) [((i,j), True) | i <- [0..height], j <- [0..width]]
    corruptMemory corruption = [((x,y), False) | (y,x) <- corruption]
    pCorruption = sepBy ((,) <$> (decimal <* char ',') <*> decimal) eol

binSearch :: String -> Int -> Int -> (Int, Int)
binSearch input lo hi =   
  if hi == lo 
    then 
      let
        finalCandidates =  [lo -1, lo, lo+1]
        rams = map (\x -> parse' input $ pRam x) finalCandidates
        runs = catMaybes $ map (\(r,i) -> (\_ -> i) <$> dijkstra r) $ zip rams finalCandidates
        res = (last runs)
      in  
        parse' input $ fmap (!! res) $ sepBy ((,) <$> (decimal <* char ',') <*> decimal) eol
    else 
      let 
        mid = (lo + hi) `div` 2
        ram = parse' input $ pRam mid
      in 
        case dijkstra ram of
        Just _ -> binSearch input (mid + 1) hi
        Nothing -> binSearch input lo (mid - 1)


dijkstra :: RAM -> Maybe Int
dijkstra ram = 
  let
    q = PQ.push (0,(0,0)) PQ.empty
    visited = (S.singleton (0,0))
    end = (height, width)
  in
    dijkstra' ram visited q end

dijkstra' :: RAM -> Set Pos ->  PQueue Int Pos -> Pos -> Maybe Int
dijkstra' ram visited q end = PQ.pop q >>= \((s,(x,y)), q') ->
  if (x,y) == end
    then Just s
    else 
      let 
        candidates = zip (repeat (s+1)) [(x+1,y), (x-1,y), (x,y+1), (x, y-1)]
        validMove (a,b) = a >= 0 && b >= 0 && a <= height && b <= width && 
                          S.notMember (a,b) visited && (ram A.! (a,b))
        validMoves = filter (validMove . snd)  candidates
        visited' = foldl' (\s' (_,p) -> S.insert p s') visited validMoves
        q'' = PQ.pushAll validMoves q'
      in
        dijkstra' ram visited' q'' end

day18 :: IO (Int, Int)
day18 = do
  input <- readFile "src/inputs/day18.txt"
  let p1 = fromJust $ parse' input $ dijkstra <$> pRam 2861
  let p2 = binSearch input 0 3450 
  pure $ (p1, fst p2 + snd p2)
