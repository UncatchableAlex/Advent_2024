module Y2025.DAYS.Day4 where
  
import Text.Megaparsec (many, choice, sepBy)
import Text.Megaparsec.Char (char, eol)
import UTIL.Parsers (Parser, parse')
import UTIL.Util (arrayify2d, get2dArrayNeighbors, count)
import Data.Array (Array)
import qualified Data.Array as A

pInput :: Parser (Array (Int, Int) Char)
pInput = arrayify2d <$> sepBy (many $ choice [char '@', char '.']) eol

forkliftableRolls :: Array (Int, Int) Char -> (Int, Int) -> Bool
forkliftableRolls arr i = (arr A.! i == '@') && ((< 4) . length . filter (== '@') $ map snd $ get2dArrayNeighbors arr i)

removeForkliftableRolls :: Array (Int, Int) Char -> Int
removeForkliftableRolls arr
  | length flrs == 0 = 0 
  | otherwise = (length flrs) + removeForkliftableRolls arr'
  where 
    flrs = filter (forkliftableRolls arr) (A.indices arr)
    arr' = arr A.// [(i, '.') | i <- flrs]


day4 :: IO (Int, Int)
day4 = do
  content <- readFile "src/y2025/inputs/day4.txt"
  let p1 = parse' content $ do
        arr <- pInput
        pure $ count (forkliftableRolls arr) (A.indices arr)
  let p2 = parse' content $ removeForkliftableRolls <$> pInput
  pure (p1, p2)