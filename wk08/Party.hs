-- CIS 194, week 8

module Party where

import Employee
import Data.Tree

-- Exercise 1

-- 1. define glCons, which adds an Employee to GuestList

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

-- 2. define a Monoid instance for GuestList
-- ???

-- 3. define moreFun, which takes two GuestLists and returns the one
-- which has more fun

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if (f1 > f2) then gl1 else gl2

-- Exercise 2

-- implement treeFold for Data.Tree

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f z t = foldl f z $ nodeList t

nodeList :: Tree a -> [a]
nodeList (Node a []) = [a]
nodeList (Node a sf) = a : (concat . map nodeList) sf

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> [(GuestList, GuestList)]
