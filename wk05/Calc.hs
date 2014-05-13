-- CIS 194, Week 05 Exercises

module Calc where

import ExprT
import Parser

-- Exercise 01

-- Write Version 1 of the calculator: an evaluator for ExprT, with the
-- signature:
--    eval :: ExprT -> Integer
-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Exercise 02

-- Write an evaluator for string expressions

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Just e -> Just (eval e)
  Nothing -> Nothing

-- Exercise 03

-- typeclass Expr

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id
