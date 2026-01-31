module Y2025 (days, day1) where

import Y2025.DAYS.Day1 (day1)

days :: [IO (Int, Int)]
days =
  [ day1
  ]