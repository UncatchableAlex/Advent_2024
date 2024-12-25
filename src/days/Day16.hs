module DAYS.Day16 (day16) where

import Data.Array (Array, (!))
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec (oneOf, sepBy, some)
import Text.Megaparsec.Char (eol)
import UTIL.PQueue (PQueue)
import qualified UTIL.PQueue as PQ
import UTIL.Parsers (Parser, parse')
import UTIL.Util (arrayify2d)

type Maze = Array (Int, Int) Char

type Pos = (Int, Int)

type Dir = (Int, Int)

type Path = [Pos]

-- this is as big as we will let our PQueue grow to. Things start getting 
-- REALLY slow past this threshold. Keep in mind, this is a crappy immutable 
-- datastructure that I made at 3 AM after reading a wikipedia article about 
-- binomial heaps.
maxQueueSize :: Int
maxQueueSize = 500

parseMaze :: Parser Maze
parseMaze = arrayify2d <$> (sepBy (some (oneOf ".#ES")) eol)

-- would this go about 200x faster with an actual priority queue? Maybe...
dijkstra :: Maze -> (Int, Int)
dijkstra maze = case step initialQ (S.singleton (start, (0, -1))) maxBound of
  Just (p1, p2Set) ->
    (p1, S.size p2Set)
  Nothing -> error "No path found"
  where
    start = case filter (\(_, x) -> x == 'S') $ A.assocs maze of
      [s] -> fst s
      _ -> error "start (S) not present in maze"
    end = case filter (\(_, x) -> x == 'E') $ A.assocs maze of
      [e] -> fst e
      _ -> error "end (E) not present in maze"

    initialQ = PQ.push (0, (start, (0, -1), [start])) PQ.empty

    step :: PQueue Int (Pos, Dir, Path) -> Set (Pos, Dir) -> Int -> Maybe (Int, Set Pos)
    step q visited best =
      do
        ((score, (pos, dir, path)), q') <- PQ.pop q
        let ms = moves pos dir score visited path
        let q'' = if PQ.size q' >= maxQueueSize then compact q' else q'
        if pos == end && (length q'' /= 0) -- check to make sure that path's still exist
          then
            if score <= best -- we found a score that was at least as good as the best!
              then
                let pathSet = S.fromList path
                    visitedNotPath = filter ((flip S.notMember pathSet) . fst) $ S.elems visited
                    otherPaths = step q'' (S.fromList visitedNotPath) score
                 in otherPaths >>= \p -> Just (score, S.union (snd p) $ S.fromList path)
              else -- we found a path that wasn't the best. we can stop looking for other paths now, 
                    -- we found all the good ones, they all suck now.
                Just (score, S.empty)
          else step (PQ.pushAll ms q'') (S.insert (pos, dir) visited) best

    moves (x, y) (dx, dy) score visited path =
      filter
        (\(_, (pos', dir, _)) -> (S.notMember (pos', dir) visited) && ((maze ! pos') /= '#'))
        ( [ (score + 1, ((x + dx, y + dy), (dx, dy), (x + dx, y + dy) : path)),
            (score + 1001, ((x - dy, y + dx), (-dy, dx), (x - dy, y + dx) : path)),
            (score + 1001, ((x + dy, y - dx), (dy, -dx), (x + dy, y - dx) : path))
          ]
        )

-- dump and repack our priority queue. If it gets too big, it slows down A LOT
compact :: PQueue Int (Pos, Dir, Path) -> PQueue Int (Pos, Dir, Path)
compact q = PQ.fromList $ map toScore $ M.assocs pathMap
  where
    fromScore (score, (pos, dir, path)) = ((score, pos, dir), path)
    toScore ((score, pos, dir), path) = (score, (pos, dir, path))
    paths = map fromScore $ PQ.extractAll q
    pathMap = M.map (S.elems . S.fromList) $ M.fromListWith (++) paths

-- expecting (95444,513)
day16 :: IO (Int, Int)
day16 = readFile "src/inputs/day16.txt" >>= \input -> 
  pure $  parse' input (dijkstra <$> parseMaze)