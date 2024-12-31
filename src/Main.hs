module Main where

import DAYS.Day1 (day1)
import DAYS.Day10 (day10)
import DAYS.Day11 (day11)
import DAYS.Day12 (day12)
import DAYS.Day13 (day13)
import DAYS.Day14 (day14)
import DAYS.Day15 (day15)
--import DAYS.Day15_Pure (day15Pure)
import DAYS.Day16 (day16)
import DAYS.Day17 (day17)
import DAYS.Day18 (day18)
import DAYS.Day19 (day19)
import DAYS.Day2 (day2)
import DAYS.Day20 (day20)
import DAYS.Day21 (day21)
import DAYS.Day22 (day22)
import DAYS.Day23 (day23)
import DAYS.Day24 (day24)
import DAYS.Day25 (day25)
import DAYS.Day3 (day3)
import DAYS.Day4 (day4)
import DAYS.Day5 (day5)
import DAYS.Day6 (day6)
import DAYS.Day7 (day7)
import DAYS.Day8 (day8)
import DAYS.Day9 (day9)

days :: [IO (Int, Int)]
days =
  [ day1,
    day2,
    day3,
    day4,
    day5,
    day6,
    day7,
    day8,
    day9,
    day10,
    day11,
    day12,
    day13,
    day14,
    day15,
  --  day15Pure,
    day16,
    day17,
    day18,
    day19,
    day20,
    day21,
    day22,
    day23,
    day24,
    day25
  ]

main :: IO ()
main = mapM_ printDay $ zip days [1 .. 25]
  where
    printDay :: (IO (Int, Int), Int) -> IO ()
    printDay (x, i) = x >>= (\y -> print ("Day " ++ (show i) ++ ":  " ++ show y))