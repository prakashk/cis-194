{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- CIS 194, week 7

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
                      deriving (Eq, Show)

-- safe list indexing function

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

-- convert given JoinList to list, ignoring the monoidal annotations

jlToList :: JoinList m a -> [a]
jlToList Empty                 = []
jlToList (Single _ a)          = [a]
jlToList (Append _ left right) = jlToList left ++ jlToList right

-- return the monoidal annotation of the root of given JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- return the "size" of the JoinList
sz :: (Sized b, Monoid b) => JoinList b a -> Int
sz = getSize . size . tag

-- Exercise 1
-- Write an append function for JoinLists that yields a new JoinList
-- whose monoidal annotation is derived from those of the two
-- arguments

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

-- Exercise 2
-- 1. Implement the function indexJ to find the JoinList element at
-- the specified index; it should satisfy the equivalence:
--     (indexJ i jl) == (jlToList jl !!? i)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                 = Nothing
indexJ i _     | i < 0         = Nothing
indexJ i jl    | i > sz jl     = Nothing
indexJ i (Single _ a)          = Just a
indexJ i (Append m left right)
  | i < sz left = indexJ i left
  | otherwise   = indexJ (i - sz left) right

-- 2. Implement the function dropJ to drop first n elements of a
-- JoinList; it should satisfy the equivalence:
--     jlToList (dropJ n jl) = drop n (jlToList jl)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a ->JoinList b a
dropJ _ Empty        = Empty
dropJ n jl | n <= 0  = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append m left right)
  | n < sz left = (dropJ n left) +++ right
  | otherwise   = dropJ (n - sz left) right

-- 3. Implement the function takeJ to return the first n elements of a
-- JoinList, dropping all other elements; it should satisfy the equivalence:
--     jlToList (takeJ n jl) == take n (jlToList jl)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a ->JoinList b a
takeJ _ Empty          = Empty
takeJ n _  | n <= 0    = Empty
takeJ n jl | n > sz jl = jl
takeJ n (Append m left right)
  | n < sz left = takeJ n left
  | otherwise   = left +++ takeJ (n - sz left) right

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4

type JLBuffer = JoinList (Score, Size) String

instance Buffer JLBuffer where

  -- toString :: JLBuffer -> String
  toString = unlines . jlToList

  -- fromString :: String -> JLBuffer
  fromString = foldr (+++) Empty . map (\s -> Single (scoreString s, Size 1) s) . lines

  -- line :: Int -> JLBuffer -> Maybe String
  line = indexJ

  -- replaceLine :: Int -> String -> JLBuffer -> JLBuffer
  replaceLine n rstr jlb =
    takeJ n jlb +++ Single (scoreString rstr, Size 1) rstr +++ dropJ (n+1) jlb

  -- numLines :: JLBuffer -> Int
  numLines = sz

  -- value :: JLBuffer -> Int
  value = scorev . fst . tag
          where scorev (Score i) = i

-- JLBuffer based editor

main = runEditor editor jlb
  where jlb = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JLBuffer
