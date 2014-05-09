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
    height Leaf             = -1
    height (Node h lt x rt) = h

    -- left Leaf            = Leaf
    -- left (Node _ lt _ _) = lt

    -- right Leaf            = Leaf
    -- right (Node _ _ _ rt) = rt

    count Leaf = 0
    count (Node _ lt _ rt) = count lt + 1 + count rt

    insert x Leaf             = Node 0 Leaf x Leaf
    insert x (Node h Leaf r Leaf) = Node (h+1) (insert x Leaf) r Leaf
    insert x (Node h Leaf r rt) = Node h (insert x Leaf) r rt
    insert x (Node h lt r Leaf) = Node h lt r (insert x Leaf)
    insert x (Node h lt r rt)
      | height lt > height rt = Node h lt r (insert x rt)
      | height lt < height rt = Node h (insert x lt) r rt
      | count lt > count rt   = Node h lt r (insert x rt)
      | count lt < count rt   = Node h (insert x lt) r rt
      | otherwise             = let newlt = insert x lt
                                    newheight = 1 + max (height newlt) (height rt)
                                in Node newheight newlt r rt
