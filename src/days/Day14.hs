module DAYS.Day14 (day14) where

import Data.List (elemIndex, partition)
import qualified Data.Map.Strict as M
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, eol, space1, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import UTIL.Parsers (Parser, parse')
import UTIL.Util (nofail)

-- import qualified Data.Set as S

type Dims = (Int, Int)

type Pos = (Int, Int)

type Robot = (Int, Int, Int, Int)

signedDecimal :: Parser Int
signedDecimal = signed (pure ()) decimal

pRobots :: Parser [Robot]
pRobots = sepBy pRobot eol
  where
    pRobot =
      (,,,)
        <$> (string "p=" *> signedDecimal)
        <*> (char ',' *> signedDecimal)
        <*> (space1 *> (string "v=") *> signedDecimal)
        <*> (char ',' *> signedDecimal)

part1 :: Dims -> [Robot] -> Int
part1 (h, w) robots = M.foldl' (*) 1 (M.fromListWith (+) $ zip quadrants $ repeat 1)
  where
    halfh = h `div` 2
    halfw = w `div` 2
    positions = map (\pos -> step (h, w) 100 pos) robots
    validPositions = filter (\(a, b) -> a /= halfw && b /= halfh) positions
    quadrants = map (\(a, b) -> (a > halfw, b > halfh)) validPositions

-- This is tricky. I assumed that he would try to be tricky and not put the tree in the middle
-- therefore, what I do is split the grid in half and shift one of the halves over the other.
-- I return the iteration with the lowest correlation that I detect. This was the first truely difficult problem
-- of the season (although day 12 part 2 was also nasty).
part2 :: Dims -> Int -> [Robot] -> Int
part2 (h, w) s robots = nofail $ elemIndex (minimum $ correlations) correlations
  where
    correlations = map correlateStep [0 .. s]
    correlateStep n = correlate (map (step (h, w) n) robots)

    correlate :: [Pos] -> Float
    correlate positions =
      let binSize = 8
          halfw = (w `div` binSize) `div` 2
          binned = map (\(x, y) -> (x `div` binSize, y `div` binSize)) positions
          (left, right) = partition (\(x, _) -> x < halfw) $ filter (\(x, _) -> x /= halfw) binned
          leftMap = M.fromListWith (+) $ zip left $ repeat 1
          rightMap = M.fromListWith (+) $ zip right $ repeat 1
          numerator = sum $ map (\((x, y), occs) -> occs * M.findWithDefault 0 (x - halfw, y) leftMap) $ M.assocs rightMap
          leftSquaredSum = sum [occs ^ (2 :: Int) | occs <- M.elems leftMap]
          rightSquaredSum = sum [occs ^ (2 :: Int) | occs <- M.elems rightMap]
          denominator = (sqrt leftSquaredSum) * sqrt rightSquaredSum
       in numerator / denominator

-- printTree :: Int -> Dims -> [Robot] -> String
-- printTree t (h,w) robots = "\n\n" ++ (show t) ++ ":\n" ++ [mapChar (i,j) | j <- [0..h], i <- [0..w]]
--   where
--     finalPosSet = S.fromList $ map (step (h,w) t) robots
--     mapChar (i,j) = case (S.member (i,j) finalPosSet, i == w) of
--       (True, _) -> '#'
--       (False, False) -> ' '
--       (False, True) -> '\n'

step :: Dims -> Int -> Robot -> (Int, Int)
step (h, w) t (x, y, dx, dy) =
  ( (t * dx + x) `mod` w,
    (t * dy + y) `mod` h
  )

-- expect (225943500, 6377)
day14 :: IO (Int, Int)
day14 = do
  input <- readFile "src/inputs/day14.txt"
  let dims = (103, 101)
  let p1 = parse' input $ part1 dims <$> pRobots
  let p2 = parse' input $ part2 dims 10000 <$> pRobots
  -- putStrLn $ parse' input $ (printTree p2 dims) <$> pRobots -- print the tree!
  pure $ (p1, p2)
