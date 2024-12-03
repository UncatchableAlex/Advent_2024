module DAYS.Day3 (day3) where 
import UTIL.Parsers (parse', lInt, Parser)
import Text.Megaparsec (anySingle, single, choice, try, manyTill, MonadParsec (eof))
import Text.Megaparsec.Char (string)

maybeIntToInt :: Maybe Int -> Int
maybeIntToInt m = case m of
  Just i -> i
  Nothing -> 0
  
part1 :: String -> Int
part1 s = case parse' (manyTill pMul eof) s of 
  Left err -> error err
  Right res -> sum $ map maybeIntToInt res
  where
    pMul :: Parser (Maybe Int)
    pMul = choice
      [ try $ do
          _ <- string "mul("
          a <- lInt <* single ','
          b <- lInt <* single ')'
          pure $ Just $ a * b, 
        Nothing <$ anySingle
      ] 



part2 :: String -> Int
part2 s = case parse' (manyTill pMul eof) s of 
  Left err -> error err
  Right res -> sum $ map maybeIntToInt res
  where
    pMul :: Parser (Maybe Int)
    pMul = choice
      [
         try $ do
          _ <- string "mul("
          a <- lInt <* single ','
          b <- lInt <* single ')'
          pure $ Just $ a * b, 
        Nothing <$ string "don't()" <* manyTill anySingle (string "do()"),
        Nothing <$ anySingle
      ] 

day3 :: IO (Int, Int)
day3 = do
  content <- readFile "src/inputs/day3.txt"
  pure $ (part1 content, part2 content)