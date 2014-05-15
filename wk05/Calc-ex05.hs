{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Expr
import Parser
import StackVM

-- Exercise 05

instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

-- Compiler for StackVM

compile :: String -> Maybe Program
compile = parseExp lit add mul
