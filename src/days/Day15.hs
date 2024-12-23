module DAYS.Day15 (day15) where

import Data.Array.ST (STUArray)
import qualified Data.Array.ST as STA
import qualified Data.Array.IArray as IA
import Data.Array.IArray (Array) 
import Text.Megaparsec (endBy, oneOf, sepBy, some)
import Text.Megaparsec.Char (eol)
import UTIL.Parsers (Parser, parse')
import UTIL.Util (pairAdd, arrayify2d)
import Control.Monad.ST (ST)
import qualified Control.Monad.Trans.Maybe as MT
import Control.Monad.Trans.Maybe (MaybeT) 
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.ST as ST

type Warehouse s = STUArray s (Int, Int) Char

type RobotMoves = String

type Pos = (Int, Int)

type Dir = (Int, Int)

parseGrid :: Parser (ST s (Warehouse s), RobotMoves)
parseGrid =
  (,)
    <$> ((STA.thaw . arrayify2d) <$> endBy (some (oneOf "#.@O[]")) eol)
    <*> (eol *> (concat <$> sepBy (some (oneOf "<>^v")) eol))

moves :: Char -> Dir
moves '<' = (0, -1)
moves '^' = (-1, 0)
moves 'v' = (1, 0)
moves '>' = (0, 1)
moves _ = error "invalid move character"

translate :: Pos -> Char -> Pos
translate p c = pairAdd p $ moves c

swap :: Pos -> Pos -> Warehouse s -> ST s (Warehouse s)
swap i j grid = do
    valI <- STA.readArray grid i
    valJ <- STA.readArray grid j
    STA.writeArray grid i valJ
    STA.writeArray grid j valI
    pure $ grid

widen :: (ST s (Warehouse s), RobotMoves) -> (ST s (Warehouse s), RobotMoves)
widen (grid, rmoves) = (grid >>= wideGrid, rmoves)
  where
    wideGrid :: Warehouse s -> ST s (Warehouse s)
    wideGrid grid' = 
      do
        (_, (x, y)) <- STA.getBounds grid'
        assocs <- STA.getAssocs grid'
        let y' = 1 + y * 2
        let eles = concat $ map widenSubList $ assocs
        let widePure = IA.array ((0, 0), (x, y')) eles :: Array (Int, Int) Char
        STA.thaw widePure

    widenSubList ((a, b), e) = case e of
        'O' -> [((a, b * 2), '['), ((a, 2 * b + 1), ']')]
        '#' -> [((a, b * 2), '#'), ((a, 2 * b + 1), '#')]
        '.' -> [((a, b * 2), '.'), ((a, 2 * b + 1), '.')]
        '@' -> [((a, b * 2), '@'), ((a, 2 * b + 1), '.')]
        err -> error $ "unrecognized character in widen sublist " ++ [err]

moveRobot :: (ST s (Warehouse s), RobotMoves) -> ST s Int
moveRobot (warehouse, rmoves) = gpsScore $ fst <$> folds
    where
      folds = foldl' step initial rmoves

      initial = (,) <$> warehouse <*> start 

      gps :: ((Int, Int), Char) -> Int 
      gps ((x, y), e) = (y + 100 * x) * (fromEnum $ e `elem` "O[")
      
      gpsScore :: ST s (Warehouse s) -> ST s Int
      gpsScore grid = (sum . (map gps)) <$> (grid >>= STA.getAssocs)

      start = (fst . (!! 0) . (filter (\(_, e) -> e == '@'))) <$> (warehouse >>= STA.getAssocs)


step :: (ST s (Warehouse s, Pos)) -> Char -> (ST s (Warehouse s, Pos))
step st move = do
  (grid, pos) <- st
  let pos' = translate pos move
  result <- MT.runMaybeT $ doMove grid pos move
  case result of
        Just grid' -> pure (grid', pos')
        Nothing -> pure (grid, pos)

doMove :: Warehouse s -> Pos -> Char -> MaybeT (ST s) (Warehouse s)
doMove grid (x, y) d = do
  curr <- lift $ STA.readArray grid (x,y)
  case curr of
    '.' -> pure grid

    _ | curr `elem` "O@" || (curr `elem` "[]" && d `elem` "<>") -> 
      (doMove grid (x', y') d) >>= (lift . swap (x, y) (x', y'))

    '#' -> MT.hoistMaybe Nothing

    _ | curr `elem` "[]" -> do
      forward <- doMove grid (x', y') d
      forwardSwapped <- lift $ swap (x, y) (x', y') forward
      let y'' = if curr == '[' then y + 1 else y - 1
      adjacent <- doMove forwardSwapped (x', y'') d
      lift $ swap (x, y'') (x', y'') adjacent

    _ -> error $ "unrecognized character " ++ (show $ curr)
  where
    (x', y') = translate (x, y) d

-- expecting (1516281,1527969)
day15 :: IO (Int, Int)
day15 = do
  input <- readFile "src/inputs/day15.txt"
  let p1 = parse' input $ moveRobot <$> parseGrid
  let p2 = parse' input $ moveRobot <$> (widen <$> parseGrid)
  pure $ (ST.runST p1, ST.runST p2)--p2)