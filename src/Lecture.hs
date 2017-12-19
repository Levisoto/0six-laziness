module Lecture where

import Prelude hiding (foldr,foldl)
import Data.Array

foldr f z []     = z
foldr f z (x:xs) = x `f` foldr f z xs

foldl f z []     = z
foldl f z (x:xs) = let z' = z `f` x
                    in foldl f z' xs

foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs

sum1 = foldr (+) 0
sum2 = foldl (+) 0
sum3 = foldl' (+) 0

veryBigList = [1..1000000]

try1 = sum1 veryBigList
try2 = sum2 veryBigList
try3 = sum3 veryBigList

----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
knapsack :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

datalist vs ws maxW = m
       where
        numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example = knapsack [3,4,5,8,10] [2,3,4,5,9] 20
