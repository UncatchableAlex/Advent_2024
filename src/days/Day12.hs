module DAYS.Day12 (day12) where 

day12 :: IO (Int, Int)
day12 = do
  _ <- readFile "src/inputs/day12.txt"
  pure $ (-1, -1)