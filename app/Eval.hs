module Eval where

import Ast
import Value

eval :: Expr -> Value
eval (Integer x) = Int x
eval (FloatLit x) = FloatPoint x
eval (StringLit s) = String s
eval (Char c) = CharLit c
eval (Bool b) = Boolean b
eval (Negation n) = negateValue (eval n)
eval (Not n) = notValue (eval n)
eval (Add l r) = addValues (eval l) (eval r)
eval (Sub l r) = subValues (eval l) (eval r)
eval (Mul l r) = mulValues (eval l) (eval r)
eval (Div l r) = divValues (eval l) (eval r)
eval (Pow l r) = powValues (eval l) (eval r)
eval (Mod l r) = modValues (eval l) (eval r)
eval (Concat l r) = concatValues (eval l) (eval r)
eval (Id x) = String x
