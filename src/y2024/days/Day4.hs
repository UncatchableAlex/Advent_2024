module Y2024.DAYS.Day4 (day4, diagonalize, part1, xmasOnRows) where 
import Data.List (transpose)
import Data.IntMap.Strict(IntMap, fromListWith, toList)

xmasOnRows :: [[Char]] -> Int
xmasOnRows x = sum $ map xmasOnRows' x 
  where
    xmasOnRows' :: [Char] -> Int
    xmasOnRows' ('X':'M':'A':'S':xs) = 1 + xmasOnRows' ('S':xs)
    xmasOnRows' ('S':'A':'M':'X':xs) = 1 + xmasOnRows' ('X':xs)
    xmasOnRows' (_:xs) = xmasOnRows' xs
    xmasOnRows' [] = 0

-- Fast stateful solution. We hide state with the map, but our result depends on insertion order into the map
diagonalize :: [[Char]] -> [[Char]]
diagonalize [] = []
diagonalize (x:xs) = map snd $ toList diagMap where
  diagMap :: IntMap [Char]
  diagMap = fromListWith (++) diagList
  diagList :: [(Int, [Char])]
  diagList = [(i+j, [n]) | (row, i) <- zip (x:xs) [0..], (n, j) <- zip row [0..]]

part1 :: [[Char]] -> Int
part1 x = forward  + down + diags1 + diags2 where
  forward = xmasOnRows x
  down = xmasOnRows $ transpose x
  diags1 = xmasOnRows $ diagonalize x
  diags2 = xmasOnRows $ diagonalize $ reverse x

part2 :: [[Char]] -> Int
part2 [] = 0
part2 (x:xs) = sum [isXmas $ box3 i j | i <- [0..m], j <- [0..n]] where
    (m, n) = (length xs - 2, length x - 3)
    box3 :: Int -> Int -> [[Char]]
    box3 a b = map (take 3 . drop b) $ take 3 $ drop a (x:xs)

    isXmas [['M',_,'M'], [_, 'A', _], ['S', _, 'S']] = 1
    isXmas [['M',_,'S'], [_, 'A', _], ['M', _, 'S']] = 1
    isXmas [['S',_,'M'], [_, 'A', _], ['S', _, 'M']] = 1
    isXmas [['S',_,'S'], [_, 'A', _], ['M', _, 'M']] = 1
    isXmas _ = 0

-- expected output: (2571,1992)
day4 :: IO (Int, Int)
day4 = readFile "src/y2024/inputs/day4.txt" >>= (\x -> pure $ (part1 $ lines x, part2 $ lines x))