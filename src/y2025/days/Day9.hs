module Y2025.DAYS.Day9 (day9) where

import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')

type Constraint = [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Bool

pInput :: Parser [(Int, Int)]
pInput = sepBy ((,) <$> (decimal <* char ',') <*> decimal) eol

biggestTile :: Constraint -> [(Int, Int)] -> Int
biggestTile constraint ls =
  maximum
    [ (abs (a1 - b1) + 1) * (abs (a2 - b2) + 1)
    | ((a1, a2), i) <- zip ls [1::Int ..],
      ((b1, b2), j) <- zip ls [1::Int ..],
      i < j,
      constraint ls (a1,a2) (b1,b2),
      constraint ls (a1,b2) (b1,a2)
    ]

day1Constraint :: Constraint
day1Constraint _ _ _ = True 


day2Constraint :: Constraint
day2Constraint (n:ns) (v1, v2) (w1, w2) = and innerConnections &&  lastConnection
  where
    innerConnections = [admissible $ order x y | (x,y) <- zip (n:ns) ns]
    lastConnection = admissible $ order (last (n:ns)) n
    ((a1,a2), (b1, b2)) = if v1 < w1 then ((v1, v2), (w1, w2)) else ((w1, w2),(v1, v2))
    admissible ((x1,x2), (y1,y2))                                                                     --      x
      | x2 == y2 && a2 < x2 && x2 < b2 && x1 <= a1 && y1 > a1 = False                                 -- a ...... (a1,b2)
                                                                                                      -- .    y    .
                                                                                                      -- ..........b

                                                                                                      --  a..........                                                                                                
                                                                                                      --  .     x   .
      | x2 == y2 && a2 < x2 && x2 < b2 && x1 < b1 && y1 >= b1 = False                                 --(b1,a2).....b
                                                                                                      --        y

                                                                                                      --   a ..........
      | x1 == y1 && a1 < x1 && x1 < b1 && x2 <= a2 && y2 > a2 = False                                 -- x  .  y      .
                                                                                                      --   (b1,a2)....b
                                                                                                      
                                                                                                      --  a........(a1,b2)                                                                                                
      | x1 == y1 && a1 < x1 && x1 < b1 && x2 < b2 && y2 >= b2 =  False                                --  .       x .  y
                                                                                                      --  ...........b

      | otherwise = True

    -- we can reduce our total cases in "admissible" by ensuring that x and y are always ordered the same way:
    order :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
    order (j1, j2) (k1, k2)
      | j2 == k2 && j1 < k1 = ((j1, j2), (k1, k2))
      | j2 == k2 && j1 > k1 = ((k1, k2), (j1, j2))
      | j1 == k1 && j2 < k2 = ((j1, j2), (k1, k2))
      | j1 == k1 && j2 > k2 = ((k1, k2), (j1, j2))
      | otherwise = error "alex forgot a case again"

day2Constraint [] _ _ = True                                                                                              

day9 :: IO (Int, Int)
day9 = do
  content <- readFile "src/y2025/inputs/day9.txt"
  let p1 = parse' content $ biggestTile day1Constraint <$> pInput
  let p2 = parse' content $ biggestTile day2Constraint <$> pInput 
  pure (p1,p2)