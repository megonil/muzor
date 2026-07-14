module Ast where

import Data.Text hiding (show)
import Text.Printf

data Expr
  = Integer Integer
  | String Text
  | Float Double
  | Char Char
  | Bool Bool
  | Id Text
  | Negation Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Mod Expr Expr
