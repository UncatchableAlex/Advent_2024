module DAYS.Day20 (day20) where

import Data.Array (Array, (!), (//))
import qualified Data.Array as A
import Data.Char (chr, ord)
import Text.Megaparsec (oneOf, sepBy, some)
import Text.Megaparsec.Char (eol)
import UTIL.Parsers (Parser, parse')
import UTIL.Util (arrayify2d)

type Track = Array (Int, Int) Char

type Pos = (Int, Int)

nullPos :: Pos
nullPos = (-1, -1)

parseTrack :: Parser Track
parseTrack = arrayify2d <$> sepBy (some (oneOf "#.SE")) eol

shortcutThreshold :: Int
shortcutThreshold = 100

cheat :: Int -> Track -> Int
cheat maxCheat track = length $ filter (>= shortcutThreshold) $ concat $ (map shortcuts jumpStarts)
  where
    start = map fst $ filter (\(_, x) -> x == 'S') $ A.assocs track
    finish = map fst $ filter (\(_, x) -> x == 'E') $ A.assocs track

    enumeratedTrack = track // (bfs 0 nullPos (start !! 0) (finish !! 0))

    jumpStarts :: [Pos]
    jumpStarts = map fst $ filter ((/= '#') . snd) $ A.assocs enumeratedTrack

    shortcuts :: Pos -> [Int]
    shortcuts pos =
      let allPossibleJumps = concat $ map (\i -> zip (jumps pos i) (repeat i)) [1 .. maxCheat]
          onewayJumps = filter (\(x, _) -> ord (enumeratedTrack ! x) < ord (enumeratedTrack ! pos)) allPossibleJumps
       in map (\(end, i) -> (ord (enumeratedTrack ! pos) - ord (enumeratedTrack ! end) - i)) $ onewayJumps

    bfs :: Int -> Pos -> Pos -> Pos -> [(Pos, Char)]
    bfs i prev curr end =
      let nextMove = filter (\x -> track ! x /= '#' && x /= prev) $ moves curr
       in if curr == end
            then [(curr, chr i)]
            else (curr, chr i) : (bfs (i + 1) curr (nextMove !! 0) end)

    moves :: Pos -> [Pos]
    moves (x, y) = filter inBounds [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

    jumps :: Pos -> Int -> [Pos]
    jumps (x, y) i =
      let candidates = concat $ map (\j -> [(x + j, y + (i - j)), (x - j, y - (i - j)), (x - j, y + (i - j)), (x + j, y - (i - j))]) [1 .. (i - 1)]
          zeroCase = [(x, y - i), (x, y + i), (x - i, y), (x + i, y)]
       in filter (\p -> inBounds p && track ! p /= '#') (candidates ++ zeroCase)

    inBounds (x', y') =
      let ((xmin, ymin), (xmax, ymax)) = A.bounds track
       in (x' >= xmin) && (x' <= xmax) && (y' >= ymin) && (y' <= ymax)

day20 :: IO (Int, Int)
day20 = do
  input <- readFile "src/inputs/day20.txt"
  let p1 = parse' input (cheat 2 <$> parseTrack)
  let p2 = parse' input (cheat 20 <$> parseTrack)
  pure $ (p1, p2)
