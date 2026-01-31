module Y2025.DAYS.Day1 (day1) where

import UTIL.Parsers (parse', Parser)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, eol)

import Text.Megaparsec (many, choice)

pInput :: Parser [Int]
pInput = many (choice [
    (char 'R') *> decimal,
    (char 'L') *> ((* (-1)) <$> decimal)
  ] <* eol)


rotate :: (Int, Int, Int) -> Int -> (Int, Int, Int)
rotate (p1, p2, dial) turn = (p1', p2', dial')
  where
    dialmod = ((dial + turn) `rem` 100)
    dial' = 
      if dialmod < 0 
        then 100 + dialmod
        else dialmod
    p1' = p1 + (fromEnum $ dial' == 0)
    turnmod = (turn `rem` 100) 
    passZero = (turnmod + dial >= 100) || (dial > 0 && turnmod + dial <= 0) 
    p2' = p2 + (fromEnum passZero) + (abs $ turn `quot` 100)


day1 :: IO (Int, Int)
day1 = do
  content <- readFile "src/y2025/inputs/day1.txt"
  let (p1, p2, _) = parse' content $ foldl rotate (0, 0, 50) <$> pInput
  pure (p1, p2)

