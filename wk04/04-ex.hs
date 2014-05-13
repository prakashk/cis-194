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

-- --------------------------------------------------------------------------

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

-- --------------------------------------------------------------------------

-- Exercise 03: More folds!

-- 1. Implement a function using a fold
--    xor :: [Bool] -> Bool
-- which returns True if and only if there are an odd number of True
-- values contained in the input list. It does not matter how many
-- False values the input list contains. For example,
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False

xor :: [Bool] -> Bool
xor = foldr (\b a -> not b || not a) False . filter (id)

-- 2. Implement map as a fold. That is, complete the definition
-- map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr ...
-- in such a way that map’ behaves identically to the standard map
-- function.

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

-- 3. (Optional) Implement foldl using foldr.
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a

-- Exercise 04: Finding primes
-- using the Sieve of Sundaram

-- for an integer n, generate all odd prime numbers up to 2n+2
sieveSundaram :: Int -> [Int]
sieveSundaram n = 2 : (doublePlusOne $ [1..n] `minus` removables half_n)
  where
    half_n = n `div` 2
    cartProd i j = [(x,y) | x <- [1..i], y <- [1..j], x <= y]
    removables n = map (\(a,b) -> a + b + 2*a*b) $ cartProd n n
    filterNot p = filter (not . p)
    minus xs ys = filterNot (\x -> x `elem` ys) xs
    doublePlusOne = map (\x -> 2*x+1)
