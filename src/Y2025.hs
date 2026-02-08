module Y2025 (days, day1, day2, day3) where

import Y2025.DAYS.Day1 (day1)
import Y2025.DAYS.Day2 (day2)
import Y2025.DAYS.Day3 (day3)
import Y2025.DAYS.Day4 (day4)
import Y2025.DAYS.Day5 (day5)
import Y2025.DAYS.Day6 (day6)
import Y2025.DAYS.Day7 (day7)
import Y2025.DAYS.Day8 (day8)
import Y2025.DAYS.Day9 (day9)
import Y2025.DAYS.Day10 (day10)
import Y2025.DAYS.Day11 (day11)


days :: [IO (Int, Int)]
days =
  [ day1
  , day2
  , day3
  , day4
  , day5
  , day6
  , day7
  , day8
  , day9
  , day10
  , day11
  ]