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

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap f $ streamFromSeed (+1) 1
        where f x | odd x = 0
                  | otherwise = 1 + f (x `div` 2)
