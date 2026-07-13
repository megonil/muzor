module Ast where

data Expr
  = Integer Integer
  | String String
  | Float Double
  | Char Char
  | Bool Bool
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Mod Expr Expr
