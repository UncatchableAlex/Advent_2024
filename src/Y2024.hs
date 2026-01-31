module Y2024 (days, day1, day2, day3, day4, day5, day6, day7, day8, day9, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25) where

import Y2024.DAYS.Day1 (day1)
import Y2024.DAYS.Day10 (day10)
import Y2024.DAYS.Day11 (day11)
import Y2024.DAYS.Day12 (day12)
import Y2024.DAYS.Day13 (day13)
import Y2024.DAYS.Day14 (day14)
import Y2024.DAYS.Day15 (day15)
import Y2024.DAYS.Day16 (day16)
import Y2024.DAYS.Day17 (day17)
import Y2024.DAYS.Day18 (day18)
import Y2024.DAYS.Day19 (day19)
import Y2024.DAYS.Day2 (day2)
import Y2024.DAYS.Day20 (day20)
import Y2024.DAYS.Day21 (day21)
import Y2024.DAYS.Day22 (day22)
import Y2024.DAYS.Day23 (day23)
import Y2024.DAYS.Day24 (day24)
import Y2024.DAYS.Day25 (day25)
import Y2024.DAYS.Day3 (day3)
import Y2024.DAYS.Day4 (day4)
import Y2024.DAYS.Day5 (day5)
import Y2024.DAYS.Day6 (day6)
import Y2024.DAYS.Day7 (day7)
import Y2024.DAYS.Day8 (day8)
import Y2024.DAYS.Day9 (day9)

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
