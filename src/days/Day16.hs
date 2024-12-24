module DAYS.Day16 (day16) where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Debug.Trace (traceM, trace)
import Text.Megaparsec (oneOf, sepBy, some)
import Text.Megaparsec.Char (eol)
import UTIL.PQueue (PQueue)
import qualified UTIL.PQueue as PQ
import UTIL.Parsers (Parser, parse')
import UTIL.Util (arrayify2d, pairAdd)

type Maze = Array (Int, Int) Char

type Pos = (Int, Int)

type Dir = (Int, Int)

type Path = [Pos]

parseMaze :: Parser Maze
parseMaze = arrayify2d <$> (sepBy (some (oneOf ".#ES")) eol)

part1 :: Maze -> (Int, Int)
part1 maze = case step initialQ (M.singleton (start, (0, -1)) 0) of
  Just (p1, p2Set) -> (p1, S.size p2Set)
  Nothing -> error "No path found"
  where
    (start, end) = case map fst $ filter (\(_, x) -> x `elem` "ES") $ A.assocs maze of
      [s, e] -> (s, e)
      _ -> error "start/end not present in maze"

    initialQ = PQ.push (0, (start, (0, -1), [start])) PQ.empty

    step :: PQueue Int (Pos, Dir, Path) -> Map (Pos, Dir) Int -> Maybe (Int, Set Pos)
    step q visited =
      do
        ((score, (pos, dir, path)), q') <- PQ.pop q
       -- traceM (show  (score, (pos, dir)))
        let ms = moves pos dir score visited path

        -- update the visited map with all the places we just put into the pqueue (and the corresponding scores)
        let visited' = foldl' (\m (p,d,s) -> M.insert (p,d) s m) visited $ map (\(s, (p, d, _)) -> (p, d, s)) ms
        let otherSolutions = takeWhile (\(score',_) -> score == score') $ PQ.extractAll q'
        let otherPaths = S.unions $ map (\(_, (_,_,p)) -> S.fromList p) otherSolutions
        if pos == end && (length q' /= 0) -- check to make sure that path's still exist
          then Just (score,  S.union otherPaths $ S.fromList path)
          else step (PQ.pushAll ms q') visited'

    moves pos (dx, dy) score visited path =
      filter
      -- check to see that the other paths don't cost more to get to the same place
        (\(s, (pos', dir, _)) -> (M.findWithDefault s (pos', dir) visited) == s && ((maze ! pos') /= '#'))
        [ let pos' = pairAdd pos (dx, dy) in (score + 1, (pos', (dx, dy), pos' : path)),
          (score + 1000, (pos, (-dy, dx), path)),
          (score + 1000, (pos, (dy, -dx), path))
        ]

day16 :: IO (Int, Int)
day16 = do
  input <- readFile "src/inputs/day16.txt"
  let ps = parse' input (part1 <$> parseMaze)
  pure $ ps