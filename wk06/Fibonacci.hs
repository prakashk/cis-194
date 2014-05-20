{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

import Data.List (intersperse)

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3

data Stream a = SCons a (Stream a)

-- (+++) :: a -> Stream a -> Stream a
-- (+++) = SCons
-- infixr 5 +++

streamToList :: Stream a -> [a]
streamToList (SCons x xs) = x : streamToList xs

streamFromList :: [a] -> Stream a
streamFromList [] = error "empty list does not make a Stream!"
streamFromList (x:xs) = SCons x (streamFromList xs)

instance Show a => Show (Stream a) where
  show s = "[[" ++ (concat . intersperse "," . map show . take 20 . streamToList) s ++ ",...]]"

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = SCons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (SCons x s) = SCons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = SCons x $ streamFromSeed f (f x)

streamZip :: Stream a -> Stream b -> Stream (a, b)
streamZip (SCons x xs) (SCons y ys) = SCons (x, y) (streamZip xs ys)

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (SCons x xs) (SCons y ys) = SCons (f x y) (streamZipWith f xs ys)

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap f $ streamFromSeed (+1) 1
        where f x | odd x = 0
                  | otherwise = 1 + f (x `div` 2)

-- Exercise 6
-- Fibonacci series -- using a generating function

x :: Stream Integer
x = SCons 0 (SCons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = SCons n (streamRepeat 0)
  negate = streamMap negate
  (+) = streamZipWith (+)
  (*) (SCons x xs) s2@(SCons y ys) = SCons (x * y) ((streamMap (* x) ys) + (xs * s2))

instance Fractional (Stream Integer) where
  (/) s1@(SCons x xs) s2@(SCons y ys) = q
    where q = SCons (x `div` y) (streamMap (`div` y) (xs - (s1 / s2) * ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)

-- Exercise 7
-- Fibonacci series -- using matrix multiplication

data Matrix = Matrix Integer Integer Integer Integer
              deriving (Eq, Show)

instance Num Matrix where
  (*) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
    Matrix (a1*b1+a2*b3) (a1*b2+a2*b4) (a3*b1+a4*b3) (a3*b2+a4*b4)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = mat2fib $ (Matrix 1 1 1 0)^n
         where mat2fib (Matrix a b c d) = b
