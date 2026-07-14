module Eval where

import Ast
import Error
import Operate
import Value (Value (Boolean, CharLit, FloatPoint, Int, String))

liftUnary :: (Value -> Throws Value) -> Expr -> Throws Value
liftUnary f e = eval e >>= f

liftBinary :: (Value -> Value -> Throws Value) -> Expr -> Expr -> Throws Value
liftBinary f l r = liftA2 f (eval l) (eval r) >>= id

eval :: Expr -> Throws Value
eval (Integer x) = pure $ Int x
eval (FloatLit x) = pure $ FloatPoint x
eval (StringLit s) = pure $ String s
eval (Char c) = pure $ CharLit c
eval (Bool b) = pure $ Boolean b
eval (Id x) = pure $ String x
eval (Negation n) = liftUnary negateValue n
eval (Not n) = liftUnary notValue n
eval (Add l r) = liftBinary addValues l r
eval (Sub l r) = liftBinary subValues l r
eval (Mul l r) = liftBinary mulValues l r
eval (Div l r) = liftBinary divValues l r
eval (Pow l r) = liftBinary powValues l r
eval (Mod l r) = liftBinary modValues l r
eval (Concat l r) = liftBinary concatValues l r
