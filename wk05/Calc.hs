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
-- evalStr s = case parseExp Lit Add Mul s of
--   Just e -> Just (eval e)
--   Nothing -> Nothing
evalStr = fmap eval . parseExp Lit Add Mul

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

-- Exercise 04

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit i | i <= 0 = False
        | i > 0  = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer
                 deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax (id x)
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer
                 deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)


testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul -- "(3 * -4) + 5"

testInteger = testExp :: String -> Maybe Integer
testBool    = testExp :: String -> Maybe Bool
testMM      = testExp :: String -> Maybe MinMax
testSat     = testExp :: String -> Maybe Mod7
