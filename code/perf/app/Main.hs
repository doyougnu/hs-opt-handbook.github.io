{-# OPTIONS_GHC -g -ddump-stg-final -ddump-cmm -ddump-to-file #-}

module Main where

{- Perf example: Matrix Multiplication

This is a classic example of showing how memory access can destroy performance.
The example is multiplying two large matrices in column major order. In C/C++
this is inefficient because C/C++'s memory allocation is column major. Here we
reproduce this example in Haskell, and show by using the perf linux tool that
column major order produces a high degree of pipeline stalls in the CPU.

-}

import Data.Array

main :: IO ()
main = print $ mmult m1 m2

size :: Int
size = 3

-- To ensure a CAF
aRange :: [Int]
aRange = [0..size]

arrayBounds :: ((Int,Int),(Int,Int))
arrayBounds = ((0,0), (size,size))


-- Example: a 4x4 matrix

-- array (0,3)
-- [(0,array (0,3) [(0,0),(1,1),(2,2),(3,3)])
-- ,(1,array (0,3) [(0,0),(1,1),(2,2),(3,3)])
-- ,(2,array (0,3) [(0,0),(1,1),(2,2),(3,3)])
-- ,(3,array (0,3) [(0,0),(1,1),(2,2),(3,3)])
-- ]

-- array ((0,0),(3,3))
-- [((0,0),0),((0,1),1),((0,2),2),((0,3),3)
-- ,((1,0),1),((1,1),2),((1,2),3),((1,3),4)
-- ,((2,0),2),((2,1),3),((2,2),4),((2,3),5)
-- ,((3,0),3),((3,1),4),((3,2),5),((3,3),6)
-- ]

m1 = array arrayBounds [((i,j), i + j) | i <- aRange, j <- aRange]

m2 = m1

mmult :: (Ix i, Num a) => Array (i,i) a -> Array (i,i) a -> Array (i,i) a
mmult x y
  | x1 /= y0 || x1' /= y0'  = error "range mismatch"
  | otherwise               = array ((x0,y1),(x0',y1')) l
  where
    ((x0,x1),(x0',x1')) = bounds x
    ((y0,y1),(y0',y1')) = bounds y
    ir = range (x0,x0')
    jr = range (y1,y1')
    kr = range (x1,x1')
    l  = [((i,j), sum [x!(i,k) * y!(k,j) | k <- kr]) | i <- ir, j <- jr]
