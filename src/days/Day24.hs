module DAYS.Day24 (day24) where 

day24 :: IO (Int, Int)
day24 = do
  _ <- readFile "src/inputs/day24.txt"
  pure $ (-1, -1)