{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified Data.Map as M
import Expr
import Parser

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
                deriving (Eq, Show)

type EvalVarMap = (M.Map String Integer -> Maybe Integer)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

class HasVars a where
  var :: String -> a

instance HasVars VarExprT where
  var = Var

instance HasVars EvalVarMap where
  var = M.lookup

myLift :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
myLift f Nothing _         = Nothing
myLift f _ Nothing         = Nothing
myLift f (Just a) (Just b) = Just (f a b)

instance Expr EvalVarMap where
  lit x   = (\_ -> Just x)
  add x y = (\m -> myLift (+) (x m) (y m))
  mul x y = (\m -> myLift (*) (x m) (y m))

withVars :: [(String, Integer)]
            -> EvalVarMap
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
