{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE FlexibleInstances #-}
module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Problem 1 ---------------------------------------------------------
----------------------------------------------------------------------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n<-[1..]]

-- Problem 2 ---------------------------------------------------------
----------------------------------------------------------------------
fibs2 :: [Integer]
fibs2 = a
  where
    a = 0:1:[a!!(n-1)+a!!(n-2) | n <- [2..]]

data Stream a = Cons a (Stream a)

-- Problem 3 ---------------------------------------------------------
----------------------------------------------------------------------
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:(streamToList xs)

instance Show a => Show (Stream a) where
  show = show.take 20.streamToList

-- Problem 4 ---------------------------------------------------------
----------------------------------------------------------------------
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x) 

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs 

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y = Cons y (streamFromSeed f (f y))

-- Problem 5 ---------------------------------------------------------
----------------------------------------------------------------------
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap timesPerTwo str
  where
    str = streamFromSeed (+1) 1

timesPerTwo :: Double -> Integer
timesPerTwo n 
  | even (round n) = 1 + timesPerTwo (n/2)
  | otherwise = 0

-- Problem 6 ---------------------------------------------------------
----------------------------------------------------------------------
x :: Stream Integer 
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate = streamMap (*(-1))
  (+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs + ys)
  (*) (Cons x xs) sc@(Cons y ys) = Cons (x*y) (streamMap (*x) ys + (xs * sc))

instance Fractional (Stream Integer) where
  (/) (Cons x xs) (Cons y ys) = m
    where
      m = Cons (x `div` y) (streamMap (`div` y) (xs - m * ys))

fibs3 :: Stream Integer
fibs3 = x/(1-x-x*x)

