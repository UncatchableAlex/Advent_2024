module Y2025.DAYS.Day7 (day7) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Megaparsec (choice, sepBy, some)
import Text.Megaparsec.Char (char, eol)
import UTIL.Parsers (Parser, parse')
import UTIL.Util (count, extractPos2d)

pInput :: Parser [[Char]]
pInput =
  let pLine = some $ choice [char '.', char '^', char 'S']
   in sepBy pLine eol

bothParts :: [[Char]] -> (Int, Int)
bothParts ls = (p1, p2)
  where
    -- A set of the coordinates of every splitter. 
    splitters = S.fromList $ extractPos2d '^' ls

    -- how each beam moves when the simulation is stepped. Either straight down or dividing
    -- if it hits a splitter
    shiftBeam ((i, j), a) = 
      if (i + 1, j) `S.member` splitters 
        then [((i + 1, j - 1), a), ((i + 1, j + 1), a)] 
        else [((i + 1, j), a)]

    -- we will keep track of how many beams are in each spot
    initBeam = M.fromList $ zip (extractPos2d 'S' ls) (repeat 1)

    -- iterate all beams one step
    step :: (Int, Map (Int, Int) Int) -> (Int, Map (Int, Int) Int)
    step (splits, beams) = (splits', beams')
      where
        -- combine entries for beams that share the same spot
        beams' = M.fromListWith (+) $ concat $ map shiftBeam $ M.assocs beams

        -- update how many splitters we encounter
        splits' = splits + (count (\(i, j) -> (i + 1, j) `S.member` splitters) $ M.keys beams')

    -- every beam that we track moves one step closer to the bottom with each step. Therefore, we 
    -- need only to take m steps where the grid is mxn
    (p1, p2Map) = foldl' (\b _ -> step b) (0, initBeam) [1 .. (length ls)] -- iterate step m times
    p2 = sum $ M.elems p2Map

day7 :: IO (Int, Int)
day7 = do
  content <- readFile "src/y2025/inputs/day7.txt"
  pure $ parse' content $ bothParts <$> pInput
