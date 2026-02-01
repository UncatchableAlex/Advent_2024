module Y2025.DAYS.Day5 (day5) where

import Text.Megaparsec (sepBy, some)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')
import Data.Maybe (catMaybes)
import Data.List (nub)

pInput :: Parser ([(Int, Int)], [Int])
pInput = (,) <$> (pRanges <* eol) <*> pItems 
  where
    pRange = (,) <$> (decimal <* char '-') <*> decimal
    pRanges = some (pRange <* eol)
    pItems = sepBy decimal eol

part1 :: [(Int, Int)] -> [Int] -> Int
part1 ranges items = length $ nub $ catMaybes partial
  where
    partial = [if a <= x && x <= b then Just x else Nothing | (a,b) <- ranges, x <- items]

part2 :: [(Int, Int)] -> Int
part2 ranges = 
  let nonOverlappingRanges = foldl' updateRanges [] ranges 
  in sum $ map (\(a,b) -> b - a + 1) nonOverlappingRanges
    

updateRanges :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
updateRanges ((x,y):xs) (a,b)
  | x <= a && y >= b = (x,y):xs                     -- x...........y
                                                    --    a----b

  | x >= a && y <= b = updateRanges xs (a, b)       --    x....y
                                                    -- a-----------b
                                              
  | x < a && y < a = (x,y):updateRanges xs (a,b)    -- x....y  a-----b no overlap

  | x > b && y > b = (x,y):updateRanges xs (a,b)    -- a-----b x.....y no overlap

  | x <= a && y <= b = updateRanges xs (x,b)        --  x.........y
                                                    --       a--------b

  | x >= a && y >= b = updateRanges xs (a,y)        --       x........y
                                                    --  a--------b

  | otherwise = error $ "Alex forgot to think of a case?"
  
updateRanges [] (a,b) = [(a,b)] 


day5 :: IO (Int, Int)
day5 = do
  content <- readFile "src/y2025/inputs/day5.txt"
  let p1 = parse' content $ (uncurry part1) <$> pInput 
  let p2 = parse' content $ part2 <$> fst <$> pInput
  
  pure (p1, p2)
