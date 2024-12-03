module Main where

import DAYS.Day1 (day1)
import DAYS.Day2 (day2)
import DAYS.Day3 (day3)
import DAYS.Day4 (day4)
import DAYS.Day5 (day5)
import DAYS.Day6 (day6)
import DAYS.Day7 (day7)
import DAYS.Day8 (day8)
import DAYS.Day9 (day9)
import DAYS.Day10 (day10)

days :: [IO (Int, Int)]
days =  [day1, day2, day3, day4, day5, day6, day7, day8, day9, day10]

main :: IO ()
main = mapM_ printDay $ zip days [1..10]
  where
    printDay :: (IO (Int, Int), Int) -> IO ()
    printDay (x, i) = x >>= (\y -> print ("Day " ++ (show i) ++ ":  " ++ show y))