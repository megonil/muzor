module Ast where

import Data.Text hiding (show)

data Expr
  = Integer Integer
  | StringLit Text
  | FloatLit Double
  | Char Char
  | Bool Bool
  | Id Text
  | Negation Expr
  | Not Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Mod Expr Expr
  | Concat Expr Expr
