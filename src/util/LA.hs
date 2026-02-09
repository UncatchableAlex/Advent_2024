module UTIL.LA (gausselim, matvec) where

import Data.Array (Array, (!), (//))
import qualified Data.Array as A

-- https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode
gausselim :: Array (Int, Int) Rational -> (Array (Int, Int) Rational, [Int])
gausselim ref = helper 0 0 ref
  where
    ((_,_),(m,n')) = A.bounds ref
    n = n'-1 -- ignore the augmented column
    helper :: Int -> Int -> Array (Int, Int) Rational -> (Array (Int, Int) Rational, [Int])
    helper h k ref'
      | k > n || h > m = (ref', [k..n])
      | x == 0 = 
        let (a, frees) = helper h (k+1) ref'
        in (a, k:frees)
      | otherwise = helper (h+1) (k+1) $ foldl' rule3Row swappedArr [h+1..m]
      where
          (x, imax) = maximum [(abs $ ref' ! (i, k), i) | i <- [h..m]]
          swappedArr = ref' // (concat [[((h,j), ref' ! (imax,j)), ((imax,j), ref' ! (h,j))] | j <- [0..n']])
          
          f:: Int -> Array (Int, Int) Rational -> Rational
          f i a = a!(i,k) / a!(h,k)

          rule3Row :: Array (Int, Int) Rational -> Int -> Array (Int, Int) Rational
          rule3Row a i = a // (((i,k),0):[((i,j), a!(i,j) - (a!(h,j) * (f i a))) | j <- [k+1..n']])


matvec :: Num a => Array (Int, Int) a -> Array Int a -> [a]
matvec a x = 
  let ((mlo,nlo), (mhi,nhi)) = A.bounds a 
  in [sum [a!(i,j) * x!j | j <- [nlo..nhi]] | i <- [mlo..mhi]]