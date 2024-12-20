module DAYS.Day3 (day3) where 
import UTIL.Parsers (parse', Parser)
import Text.Megaparsec (anySingle, single, choice, try, manyTill, MonadParsec (eof))
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

pMul :: Parser Int
pMul = try $ do
  a <-  string "mul(" *> decimal <* single ','
  b <- decimal <* single ')'
  pure $ a * b
  
part1 :: String -> Int
part1 s = sum $ parse' s (manyTill pIn eof)
  where
    pIn :: Parser Int
    pIn = choice [pMul, 0 <$ anySingle] 

part2 :: String -> Int
part2 s = sum $ parse' s (manyTill pIn eof)
  where
    pIn :: Parser Int
    pIn = choice [pMul, pSkip, 0 <$ anySingle] 
    pSkip = 0 <$ string "don't()" <* manyTill anySingle (string "do()")

day3 :: IO (Int, Int)
day3 = readFile "src/inputs/day3.txt" >>= (\x -> pure $ (part1 x, part2 x))