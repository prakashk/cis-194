module Calc where

import Expr
import Parser
import StackVM

-- Exercise 05

instance Expr Program where
  lit x = [PushI x]
  add x y = [PushI x, PushI y, Add]
  mul x y = [PushI x, PushI y, Mul]

-- Compiler for StackVM

compile :: String -> Maybe Program
compile = concat $ parseExp Lit Add Mul
