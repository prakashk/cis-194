-- CIS 194, week 8

module Party where

import Employee

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
