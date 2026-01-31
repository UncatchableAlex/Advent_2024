module Y2024.DAYS.Day21 (day21) where

import Data.Array ((!), (//))
import qualified Data.Array as A
import Debug.Trace (trace)
import Text.Megaparsec (sepBy, some)
import Text.Megaparsec.Char (char, digitChar, eol)
import UTIL.Parsers (Parser, parse')

type Seq = [Int]

type Up = Int

type Down = Int

type Left = Int

type Right = Int

type Enter = Int

type Moves = (Up, Down, Left, Right, Enter)

pSequence :: Parser [Seq]
pSequence = sepBy (some (digitChar >>= (pure . read . (: []))) <* char 'A') eol

keyMap :: [(Int, (Int, Int))]
keyMap = zip [-1 .. 9] [(0, 2), (0, 1), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2), (3, 0), (3, 1), (3, 2)]

-- vecDir :: Int -> Int -> Maybe Moves
-- vecDir from to = do
--   (a, b) <- lookup from keyMap
--   (c, d) <- lookup to keyMap
--   let up = if c < a then 0 else c - a
--       down = if c > a then 0 else a - c
--       right = if d < b then 0 else d - b
--       left = if d > b then 0 else b - d
--    in pure $ map fromEnum [up > 0 && left > 0, up > 0 && right > 0, down > 0 && left > 0, down > 0 && right > 0, up + down + left + right]


add2DArrays :: (A.Ix i, Num a) => A.Array i a -> A.Array i a -> A.Array i a
add2DArrays arr1 arr2 = A.array (A.bounds arr1) [(i, arr1 ! i + arr2 ! i) | i <- A.indices arr1]


-- vecAdd sums the corresponding components of each tuple in the list of Moves
vecAdd :: [Moves] -> Moves
vecAdd =
  foldl
    ( \(u1, d1, l1, r1, e1) (u2, d2, l2, r2, e2) ->
        (u1 + u2, d1 + d2, l1 + l2, r1 + r2, e1 + e2)
    )
    (0, 0, 0, 0, 0) -- Initial accumulator with all components set to 0

step :: Moves -> Moves
step (u, d, l, r, e) = (d + l + r, d + l + r, u + d + (2*l), u + d + (2*l), u+d+l+r+e) -- vecAdd [(0, 0, u, u, 0), (d, d, d, d, 0), (l, l, 2 * l, 2 * l, 0), (r, r, 0, 0, 0), (0, 0, 0, 0, e)]

day21 :: IO (Int, Int)
day21 = do
  input <- readFile "src/y2024/inputs/day21.txt"
  --let res = parse' input (map (processMoves 3 . processSeq) <$> pSequence)
  --print res
  pure $ (-1, -1)
  where
    processMoves :: Int -> Moves -> Int
    processMoves n moves = 
      let (u,d,l,r,e) = (iterate step moves) !! n 
      in trace (show (take n (iterate step moves))) u+d+l+r+e

    -- processSeq :: Seq -> Moves
    -- processSeq s = case mapM (uncurry vecDir) (zip (-1 : s ++ [-1]) (s ++ [-1])) of
    --   Nothing -> error "Invalid sequence"
    --   Just moves -> let
    --     res = vecAdd $ moves
    --     in trace (show moves) res 
