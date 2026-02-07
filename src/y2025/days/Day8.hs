module Y2025.DAYS.Day8 (day8) where

import Data.List (sort, sortBy)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')
import UTIL.Util (nofail, euclDist3)
import Data.Ord (comparing)


type Coord = (Double, Double, Double)

pInput :: Parser [Coord]
pInput =
  let pLine = (,,) <$> (decimal <* char ',') <*> (decimal <* char ',') <*> decimal
   in sepBy pLine eol

orderedDists :: [Coord] -> [(Double, Coord, Coord)]
orderedDists ls =
  sort
    [ (euclDist3 a b, a, b)
    | (a, i) <- zip ls [1 .. length ls],
      (b, j) <- zip ls [1 .. length ls],
      i < j
    ]

connect :: Int -> Int -> [Coord] -> (Int, Int)
connect n m ls = (part1, part2)
  where
    dists = orderedDists ls
    addToCircuit :: Coord -> Coord -> [[Coord]] -> [[Coord]]
    addToCircuit a b (circ : circs)
      | a `elem` circ && b `elem` circ = (circ : circs)
      | a `elem` circ = (bCirc ++ circ) : bOthers
      | b `elem` circ = (aCirc ++ circ) : aOthers
      | otherwise = circ : (addToCircuit a b circs)
      where
        (bCirc, bOthers) = lookupRemove circs b
        (aCirc, aOthers) = lookupRemove circs a
    addToCircuit a b [] = [[a, b]]
    p1 circs (_, a, b) = addToCircuit a b circs
    circuits = foldl' p1 [] (take n dists)
    part1 = product $ take m $ sortBy (comparing negate) $ map length circuits

    -- okay now we do part 2
    p2 (coord1, coord2, circs) (_, a, b) =
      let circs' = addToCircuit a b circs
       in case (coord1, coord2) of
            (Nothing, Nothing) -> 
              if length circs' == 1 && length (circs' !! 0) == length ls
                then (Just a, Just b, circs') 
                else (Nothing, Nothing, circs')
            _ -> (coord1, coord2, circs)

    (lastCoord1, lastCoord2,_) = foldl' p2 (Nothing, Nothing, []) dists
    part2 = round $ nofail $ (\(x,_,_) (y,_,_) -> x*y) <$> lastCoord1 <*> lastCoord2

-- for coordinate X, find circuit C in Coordinate list Cs where X is in C. Return C and Cs / C 
lookupRemove :: (Eq a) => [[a]] -> a -> ([a], [[a]])
lookupRemove (x : xs) y
  | y `elem` x = (x, xs)
  | otherwise = let (a, b) = lookupRemove xs y in (a, x : b)
lookupRemove [] y = ([y], [])

day8 :: IO (Int, Int)
day8 = do
  content <- readFile "src/y2025/inputs/day8.txt"
  pure $ parse' content $ connect 1000 3 <$> pInput
