module DAYS.Day15_Pure (day15Pure) where

import Data.Array (Array, (!), (//))
import qualified Data.Array as A
import Text.Megaparsec (endBy, oneOf, sepBy, some)
import Text.Megaparsec.Char (eol)
import UTIL.Parsers (Parser, parse')
import UTIL.Util (arrayify2d, pairAdd)

type Warehouse = Array (Int, Int) Char

type RobotMoves = String

type Pos = (Int, Int)

type Dir = (Int, Int)

parseGrid :: Parser (Warehouse, RobotMoves)
parseGrid =
  (,)
    <$> (arrayify2d <$> endBy (some (oneOf "#.@O[]")) eol)
    <*> (eol *> (concat <$> sepBy (some (oneOf "<>^v")) eol))

moves :: Char -> Dir
moves '<' = (0, -1)
moves '^' = (-1, 0)
moves 'v' = (1, 0)
moves '>' = (0, 1)
moves _ = error "invalid move character"

translate :: Pos -> Char -> Pos
translate p c = pairAdd p $ moves c

swap :: Pos -> Pos -> Warehouse -> Warehouse
swap i j grid = grid // [(i, grid ! j), (j, grid ! i)]

widen :: (Warehouse, RobotMoves) -> (Warehouse, RobotMoves)
widen (grid, rmoves) = (A.array ((0, 0), (x, y')) eles, rmoves)
  where
    (_, (x, y)) = A.bounds grid
    y' = 1 + y * 2
    eles = concat $ map widenSubList $ A.assocs grid
    widenSubList ((a, b), e) = case e of
      'O' -> [((a, b * 2), '['), ((a, 2 * b + 1), ']')]
      '#' -> [((a, b * 2), '#'), ((a, 2 * b + 1), '#')]
      '.' -> [((a, b * 2), '.'), ((a, 2 * b + 1), '.')]
      '@' -> [((a, b * 2), '@'), ((a, 2 * b + 1), '.')]
      err -> error $ "unrecognized character in widen sublist " ++ [err]

moveRobot :: (Warehouse, RobotMoves) -> Int
moveRobot (warehouse, rmoves) = gpsScore $ fst $ foldl' step (warehouse, start) rmoves
  where
    gpsScore grid = sum $ map (\((x, y), e) -> (y + 100 * x) * (fromEnum $ e `elem` "O[")) $ A.assocs grid
    start = fst $ (filter (\(_, e) -> e == '@') $ A.assocs warehouse) !! 0

step :: (Warehouse, Pos) -> Char -> (Warehouse, Pos)
step (grid, pos) move =
  let pos' = translate pos move
   in case doMove grid pos move of
        Just grid' -> (grid', pos')
        Nothing -> (grid, pos)

doMove :: Warehouse -> Pos -> Char -> Maybe Warehouse
doMove grid (x, y) d
  | curr == '.' = Just grid
  | curr `elem` "O@" || (curr `elem` "[]" && d `elem` "<>") = (doMove grid (x', y') d) >>= pure . (swap (x, y) (x', y'))
  | curr == '#' = Nothing
  | curr `elem` "[]" = do
      forward <- doMove grid (x', y') d
      let forwardSwapped = (swap (x, y) (x', y') forward)
      let y'' = if curr == '[' then y + 1 else y - 1
      adjacent <- doMove forwardSwapped (x', y'') d
      pure $ swap (x, y'') (x', y'') adjacent
  | otherwise = error $ "unrecognized character " ++ (show $ curr)
  where
    curr = grid ! (x, y)
    (x', y') = translate (x, y) d

-- expecting (1516281,1527969)
day15Pure :: IO (Int, Int)
day15Pure = do
  input <- readFile "src/inputs/day15.txt"
  let p1 = parse' input $ moveRobot <$> parseGrid
  let p2 = parse' input $ moveRobot <$> (widen <$> parseGrid)
  pure $ (p1, p2)