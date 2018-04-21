{-|
Module : ExprType
Description : Contains a datatype for math expressions 
Copyright : (c) Nikhail Singh @2018
License : WTFPL
Maintainer : singhn18@mcmaster.ca
Stability : experimental
Portability : POSIX.
-}
{-# LANGUAGE FlexibleInstances #-}
module ExprType where

import           Data.List

-- | A datatype for numeric expressions
data Expr a = Add (Expr a) (Expr a) -- ^ binary Addition
            | Mult (Expr a) (Expr a) -- ^ binary multiplication
            | Div (Expr a) (Expr a) -- ^ division
            | Subt (Expr a) (Expr a) -- ^ binary subtraction
            | Const a -- ^ a constant
            | Var String -- ^ a variable
            | Sin (Expr a) -- ^ Sin(expression)
            | Cos (Expr a) -- ^ Cos(expression)
            | Tan (Expr a) -- ^ Tan(expression)
            | Exp (Expr a) -- ^ e^(expression)
            | Pow (Expr a) (Expr a) -- ^ a^(expression)
            | Ln (Expr a) -- ^ Ln (expression)
  deriving (Eq)

-- ** function to get variables
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Sin e1) = getVars e1
getVars (Cos e1) = getVars e1
getVars (Tan e1) = getVars e1
getVars (Exp e1) = getVars e1
getVars (Ln e1) = getVars e1
getVars (Var ident)  = [ident]