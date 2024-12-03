module Main where

import DAYS.Day1 (day1)
import DAYS.Day2 (day2)
import DAYS.Day3 (day3)

main :: IO ()
main = do
    d1 <- day1
    putStrLn $ show d1
    d2 <- day2
    putStrLn $ show d2
    d3 <- day3
    putStrLn $ show d3
