-- CIS 194: Week 04

-- Exercise 01: Wholemeal programming

-- Reimplement each of the following functions in a more idiomatic
-- Haskell style.

-- 1. fun1 :: [Integer] -> Integer
--    fun1 [] = 1
--    fun1 (x:xs)
--      | even x = (x - 2) * fun1 xs
--      | otherwise = fun1 xs

-- 2. fun2 :: Integer -> Integer
--    fun2 1 = 0
--    fun2 n | even n = n + fun2 (n ‘div‘ 2)
--           | otherwise = fun2 (3 * n + 1)

-- Name your functions fun1’ and fun2’ respectively.

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

-- fun2' :: Integer -> Integer
-- fun2' = ...

-- ------------------------

-- Exercise 02: Folding with trees

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
                     deriving Show

foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf xs
  where
    height Leaf = -1
    height (Node h left x right) = h
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node h left r right)
      | height left > height right = Node h left r (insert x right)
      | height left < height right = Node h (insert x left) r right
      | otherwise                  = Node (h+1) (insert x left) r right