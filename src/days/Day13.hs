module DAYS.Day13 (day13) where

import Text.Megaparsec (sepBy, some)
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')

type Cost = ((Int, Int), (Int, Int))

type Objective = (Int, Int)

pClaw :: (Cost -> Objective -> Int) -> Parser Int
pClaw f = do
  a1 <- (string "Button A: X+") *> decimal
  a2 <- (string ", Y+") *> decimal
  _ <- eol
  b1 <- (string "Button B: X+") *> decimal
  b2 <- (string ", Y+") *> decimal
  _ <- eol
  c1 <- (string "Prize: X=") *> decimal
  c2 <- (string ", Y=" *> decimal)
  pure $ f ((a1, a2), (b1, b2)) (c1, c2)

-- solve a 2x2 system with cramer's rule
solveSystem :: Cost -> Objective -> Maybe (Int, Int)
solveSystem ((a1, a2), (b1, b2)) (c1, c2)
  | d == 0 || da `mod` d /= 0 || db `mod` d /= 0 = Nothing
  | otherwise = Just (da `div` d, db `div` d)
  where
    d = a1 * b2 - a2 * b1
    da = c1 * b2 - c2 * b1
    db = a1 * c2 - a2 * c1

part1 :: Cost -> Objective -> Int
part1 cost obj = case solveSystem cost obj of
  Just (r1, r2) -> 3 * r1 + r2
  Nothing -> 0

part2 :: Cost -> Objective -> Int
part2 cost (c1, c2) = case solveSystem cost (10000000000000 + c1, 10000000000000 + c2) of
  Just (r1, r2) -> 3 * r1 + r2
  Nothing -> 0

day13 :: IO (Int, Int)
day13 = do
  input <- readFile "src/inputs/day13.txt"
  let p1 = parse' input (sum <$> sepBy (pClaw part1) (some eol))
  let p2 = parse' input (sum <$> sepBy (pClaw part2) (some eol))
  pure (p1, p2)