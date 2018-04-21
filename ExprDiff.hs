{-|
Module : ExprDiff
Description : Contains a type class and instances for
differentiable expressions and the simplification of expressions. 
Copyright : (c) Nikhail Singh @2018
License : WTFPL
Maintainer : singhn18@mcmaster.ca
Stability : experimental
Portability : POSIX.
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map

-- | This is the class with methods for evaluation and differentiation of an expression
class DiffExpr a where
  -- | evaluates the expression
  eval :: Map.Map String a -> Expr a -> a
  -- | simplifies the expression 
  simplify :: Map.Map String a -> Expr a -> Expr a
  -- | partially differentiates the expression with repsect to the variable specified by the string 
  partDiff :: String -> Expr a -> Expr a 

  -- | Combines expressions for addition 
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  -- | Combines expressions for subtraction
  (!-) :: Expr a -> Expr a -> Expr a
  e1 !- e2 = simplify (Map.fromList []) $ Subt e1 e2
  -- | Combines expressions for division
  (!/) :: Expr a -> Expr a -> Expr a
  e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2
  -- | Combines expressions for multiplication
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  -- | Combines expressions for powers
  (!^) :: Expr a -> Expr a -> Expr a
  e1 !^ e2 = simplify (Map.fromList []) $ Pow e1 e2
  -- | Generates expressions for addition
  val :: a -> Expr a
  val x = Const x
  -- | creates expressions for sin
  sine ::  Expr a -> Expr a
  sine x = simplify (Map.fromList []) $ Sin x
  -- | creates expressions for cos
  cosine :: Expr a -> Expr a
  cosine x = simplify (Map.fromList []) $ Cos x
  -- | creates expressions for tan
  tangent :: Expr a -> Expr a
  tangent x = simplify (Map.fromList []) $ Tan x
  -- | creates expressions for ln
  ln1 :: Expr a -> Expr a
  ln1 x = simplify (Map.fromList []) $ Ln x
  -- | creates expressions for ln
  exp1 :: Expr a -> Expr a
  exp1 x = simplify (Map.fromList []) $ Exp x
  -- | creates expressions for variables
  var :: String -> Expr a
  var x = Var x


-- *  How to evaluate and differentiate different methods
instance (Eq a,ExprEval a,Fractional a, Floating a) => DiffExpr a where

  -- ** Evaluation
  -- | This function evaluates expressions
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Subt e1 e2)  = eval vrs e1 - eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Div e1 e2)  = eval vrs e1 / eval vrs e2
  eval vrs (Pow e1 e2)  = eval vrs e1 ** eval vrs e2
  eval vrs (Const x) = x
  eval vrs (Sin e1) = evalSin $ eval vrs e1
  eval vrs (Cos e1) = evalCos $ eval vrs e1
  eval vrs (Tan e1) = evalTan $ eval vrs e1 
  eval vrs (Ln e1) = evalLn $ eval vrs e1 
  eval vrs (Exp e1) = evalExp $ eval vrs e1
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  -- ** Simplification
  -- | This function simplifies expressions to make them easier to read and evaluate.
  -- *** constants and variables
  simplify vrs (Const a) = Const a
  simplify vrs (Var x) = case Map.lookup x vrs of
                       Just x  -> Const x
                       Nothing -> Var x                   
  -- *** addition
  simplify vrs (Add (Const x) (Const y)) = Const (x+y)
  simplify vrs (Add (Const 0) a) = simplify vrs a
  simplify vrs (Add a (Const 0)) = simplify vrs a
  simplify vrs (Add e (Var x))   = Add (simplify vrs e) (Var x)
  simplify vrs (Add (Var x) e)   = Add (Var x) (simplify vrs e)
  simplify vrs (Add (Ln a) (Ln b)) = simplify vrs (Ln (Mult (simplify vrs a) (simplify vrs b)))
  simplify vrs (Add a b) = Add (simplify vrs a) (simplify vrs b)
  -- *** subtraction
  simplify vrs (Subt (Const x) (Const y)) = Const (x-y)
  simplify vrs (Subt a (Const 0)) = simplify vrs a
  simplify vrs (Subt e (Var x))   = Subt (simplify vrs e) (Var x)
  simplify vrs (Subt (Var x) e)   = Subt (Var x) (simplify vrs e)
  simplify vrs (Subt (Ln a) (Ln b)) = simplify vrs (Ln (Div (simplify vrs a) (simplify vrs b)))
  simplify vrs (Subt a b) = Subt (simplify vrs a) (simplify vrs b)
  -- *** multiplication
  simplify vrs (Mult (Const a) (Const b)) = Const (a*b)
  simplify vrs (Mult (Const 1) a) =  simplify vrs a
  simplify vrs (Mult a (Const 1)) =  simplify vrs a
  simplify vrs (Mult (Const 0) _) =  Const 0
  simplify vrs (Mult  _ (Const 0)) = Const 0
  simplify vrs (Mult e (Var x))   = Mult (simplify vrs e) (Var x)
  simplify vrs (Mult (Var x) e)   = Mult (Var x) (simplify vrs e)
  simplify vrs (Mult (Exp a) (Exp b)) = simplify vrs (Exp (Add a b))
  simplify vrs (Mult a b) = Mult (simplify vrs a) (simplify vrs b)
  -- *** e^x
  simplify vrs (Exp (Const 0)) = Const 1
  simplify vrs (Exp (Var x)) = Exp (Var x)
  simplify vrs (Exp (Const a)) = Exp (Const a)
  simplify vrs (Exp (Ln a)) = simplify vrs a
  simplify vrs (Exp a) = Exp (simplify vrs a)
  -- *** ln
  simplify vrs (Ln (Exp a)) = simplify vrs a
  simplify vrs (Ln a) = Ln (simplify vrs a)
  -- *** division
  simplify vrs (Div _ (Const 0)) = error "cannot divide by zero"
  simplify vrs (Div (Const 0) _) = Const 0
  simplify vrs (Div (Const a) (Const b)) = Const (a/b)
  simplify vrs (Div a b) = Div (simplify vrs a) (simplify vrs b)
  -- *** power
  simplify vrs (Pow (Const 0) (Const 0)) = error "indeterminant case"
  simplify vrs (Pow (Const 1) _) = Const 1
  simplify vrs (Pow (Const 0) _) = Const 0
  simplify vrs (Pow _ (Const 0)) = Const 0
  -- *** sin
  simplify vrs (Sin a) = Sin (simplify vrs a)
  -- *** cos
  simplify vrs (Cos a) = Cos (simplify vrs a)
  -- *** Tan
  simplify vrs (Tan a) = Tan (simplify vrs a)

  -- ** Partial differentiaation
  partDiff _ (Const a) = Const 0
  partDiff v (Var x) = if x==v then (Const 1) else (Const 0)
  partDiff v (Add a b) = Add (partDiff v a) (partDiff v b)
  partDiff v (Subt a b) = Subt (partDiff v a) (partDiff v b)
  partDiff v (Mult a b) = Add x y where
        x = Mult (partDiff v a) b
        y = Mult (partDiff v b) a
  partDiff v (Div a b) = Div x y where
        x = Subt (Mult (partDiff v a) b) (Mult (partDiff v b) a)
        y = Pow b (Const 2)
  partDiff v (Pow a b) =  partDiff v (Exp (Mult b (Ln a)))
  partDiff v (Exp a) = Mult (Exp a) (partDiff v a)
  partDiff v (Ln a) = Mult (Div (Const 1) a) (partDiff v a)
  partDiff v (Sin a) = Mult (partDiff v a) (Cos a)
  partDiff v (Cos a) = Mult (partDiff v a) (Mult (Const (-1)) (Sin a))
  partDiff v (Tan a) = Div (partDiff v a) x where
        x = Pow (Cos a) (Const 2)

-- *  generalizing Class
-- | Class for generalizing expression evaluation for all numeric types 
class (Num a) => ExprEval a where 
  -- | eval for e^x
  evalExp :: a -> a
  -- | eval for Ln
  evalLn :: a -> a
  -- | eval for sin
  evalSin :: a-> a
  -- | eval for cos
  evalCos :: a -> a
  -- | eval for tan
  evalTan :: a -> a

-- | instance for floats
instance ExprEval Float where  
  evalExp x = exp x
  evalLn x = log x
  evalSin x = sin x
  evalCos x = cos x
  evalTan x = sin x /cos x

-- | instance for doubles
instance ExprEval Double where 
  evalExp x = exp x
  evalLn x = log x
  evalSin x = sin x
  evalCos x = cos x
  evalTan x = sin x /cos x

-- | instance for Ints
instance ExprEval Int where
  evalExp x = round $ exp (fromIntegral x)
  evalLn x = round  $ log (fromIntegral x)
  evalSin x = round $ sin (fromIntegral x)
  evalCos x = round $ cos (fromIntegral x)
  evalTan x = round $ sin (fromIntegral x) /cos (fromIntegral x)

-- | instance for Integers
instance ExprEval Integer where
  evalExp x = round $ exp (fromInteger x)
  evalLn x = round  $ log (fromInteger x)
  evalSin x = round $ sin (fromInteger x)
  evalCos x = round $ cos (fromInteger x)
  evalTan x = round $ sin (fromInteger x) /cos (fromInteger x)

