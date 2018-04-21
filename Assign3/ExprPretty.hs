{-|
Module : ExprPretty
Description : Contains the show instance for Expr a to allow for nicer output to the screen
Copyright : (c) Nikhail Singh @2018
License : WTFPL
Maintainer : singhn18@mcmaster.ca
Stability : experimental
Portability : POSIX.
-}
module ExprPretty where

import           ExprType

-- | a function for adding brackets
parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- | the show instance of Expr a
instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " * " ++ parens (show e2)
  show (Subt e1 e2) = parens (show e1) ++ " - " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " + " ++ parens (show e2)
  show (Div e1 e2) = parens (show e1) ++ " / " ++ parens (show e2)
  show (Pow e1 e2) = parens (show e1) ++ " ^ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
  show (Sin x) = parens $ "sin" ++ show x
  show (Cos x) = parens $ "cos" ++ show x
  show (Tan x) = parens $ "tan" ++ show x
  show (Exp x) = parens $ "e^" ++ show x
  show (Ln x) = parens $ "ln" ++ show x