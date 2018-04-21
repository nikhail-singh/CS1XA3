{-|
Module : ExprTest
Description : Contains a list of test cases and properties for evaluation. 
Copyright : (c) Nikhail Singh @2018
License : WTFPL
Maintainer : singhn18@mcmaster.ca
Stability : experimental
Portability : POSIX.
-}
{-# LANGUAGE FlexibleInstances #-}
module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck


sampleExpr1 :: Expr Double
sampleExpr1 = (var "x") !+ (var "y")

sampleExpr2 :: Expr Double
sampleExpr2 = (Const 2) !+ (Const 3)

sampleExpr3 :: Expr Double
sampleExpr3 = (Const 2.5) !+ (Const 2.5)

sampleExpr4 :: Expr Double
sampleExpr4 = (Const 3) !+ (Add (Const 2) (Const 3))

sampleExpr5 :: Expr Double
sampleExpr5 = (Const 3) !+ (Add (Const 2) (Add (Const 2) (Const 3)))

sampleExpr6 :: Expr Double
sampleExpr6 = exp1 (Const 1) 


test1 :: Double -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0
