module UTIL.Util (nofail, forceRight, arrayify2d, pairAdd) where

import Data.Array (Array)
import qualified Data.Array as A


nofail :: Maybe a -> a
nofail a = case a of
  Just a' -> a'
  Nothing -> error "fail detected in nofail"

forceRight :: Show b => Either b a -> a
forceRight a = case a of
  Right a' -> a'
  Left e -> error $ show e

arrayify2d :: [[a]] -> Array (Int, Int) a
arrayify2d arr = A.array ((0, 0), (m, n)) eles
  where
    eles = [((i, j), x) | (i, row) <- zip [0 .. m] arr, (j, x) <- zip [0 .. n] row]
    (m, n) = (length arr - 1, length (arr !! 0) - 1)

pairAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
pairAdd (a, b) (c, d) = (a + c, b + d)