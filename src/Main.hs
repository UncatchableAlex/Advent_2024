module Main where

--import Y2024
import Y2025

main :: IO ()
main = mapM_ printDay $ zip days [1 .. 25]
  where
    printDay :: (IO (Int, Int), Int) -> IO ()
    printDay (x, i) = x >>= (\y -> print ("Day " ++ (show i) ++ ":  " ++ show y))