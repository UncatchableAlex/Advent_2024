module Main where

import DAYS.Day1 (day1)

main :: IO ()
main = do
    d1 <- day1
    putStrLn $ show d1