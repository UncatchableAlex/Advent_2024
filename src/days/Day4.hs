module DAYS.Day4 (day4, diagonalize, part1, horiz) where 
import Data.List (transpose)

horiz :: [[Char]] -> Int
horiz x = sum $ map horiz' x 
  where
    horiz' :: [Char] -> Int
    horiz' ('X':'M':'A':'S':xs) = 1 + horiz' ('S':xs)
    horiz' ('S':'A':'M':'X':xs) = 1 + horiz' ('X':xs)
    horiz' (_:xs) = horiz' xs
    horiz' [] = 0

diagonalize :: [[Char]] -> [[Char]]
diagonalize [] = []
diagonalize (x:xs) = let n = (length xs + length x) - 1 in map diagonalize' [0..n] where
  diagonalize' :: Int -> [Char]
  diagonalize' d = [n | (row, i) <- zip (x:xs) [0..], (n, j) <- zip row [0..], i + j == d]

part1 :: [[Char]] -> Int
part1 x = forward  + down + diags1 + diags2 where
  forward = horiz x
  down = horiz $ transpose x
  diags1 = horiz $ diagonalize x
  diags2 = horiz $ diagonalize $ reverse x

part2 :: [[Char]] -> Int
part2 [] = 0
part2 (x:xs) = sum [isXmas $ box3 i j | i <- [0..m], j <- [0..n]] where
    (m, n) = (length xs - 2, length x - 3)
    box3 a b = [[y | (y, j) <- zip row [0..length x - 1], j >= b, j < b + 3] 
      | (row, i) <- zip (x:xs) [0..length xs], i >= a, i < a + 3]

    isXmas [['M',_,'M'], [_, 'A', _], ['S', _, 'S']] = 1
    isXmas [['M',_,'S'], [_, 'A', _], ['M', _, 'S']] = 1
    isXmas [['S',_,'M'], [_, 'A', _], ['S', _, 'M']] = 1
    isXmas [['S',_,'S'], [_, 'A', _], ['M', _, 'M']] = 1
    isXmas _ = 0

-- expected output: (2571,1992)
day4 :: IO (Int, Int)
day4 = readFile "src/inputs/day4.txt" >>= (\x -> pure $ (part1 $ lines x, part2 $ lines x))