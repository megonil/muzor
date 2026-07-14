{-# LANGUAGE RankNTypes #-}

module Operate where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Fixed (mod')
import Data.Text (pack)
import qualified Data.Text as T
import Error
import Value

unpackBool :: Value -> Throws Bool
unpackBool (Boolean x) = pure $ x
unpackBool notABool = throwError $ TypeMismatch "bool" notABool

numericOp :: (MzInt -> MzInt -> MzInt) -> (MzFloat -> MzFloat -> MzFloat) -> Value -> Value -> Throws Value
numericOp op _ (Int l) (Int r) = pure $ Int $ l `op` r
numericOp _ op (FloatPoint l) (FloatPoint r) = pure $ FloatPoint $ l `op` r
numericOp _ _ notAInt notAInt2 = throwError $ WrongOperands "binary numeric op" [notAInt, notAInt2]

unaryNumericOp :: (MzInt -> MzInt) -> (MzFloat -> MzFloat) -> Value -> Throws Value
unaryNumericOp op _ (Int i) = pure $ Int $ op i
unaryNumericOp _ op (FloatPoint f) = pure $ FloatPoint $ op f
unaryNumericOp _ _ notNum = throwError $ WrongOperands "unary numeric op" [notNum]

exactNumericOp :: (forall a. (Num a) => a -> a -> a) -> Value -> Value -> Throws Value
exactNumericOp op = numericOp op op

exactUnaryOp :: (forall a. (Num a) => a -> a) -> Value -> Throws Value
exactUnaryOp op = unaryNumericOp op op

negateValue :: Value -> Throws Value
negateValue = exactUnaryOp (\x -> -x)

notValue :: Value -> Throws Value
notValue (Boolean x) = pure $ Boolean $ not x
notValue notABool = throwError $ WrongOperands "! (not) operator" [notABool]

addValues :: Value -> Value -> Throws Value
addValues = exactNumericOp (+)

subValues :: Value -> Value -> Throws Value
subValues = exactNumericOp (-)

mulValues :: Value -> Value -> Throws Value
mulValues (String s) (Int times)
  | times < 0 = throwError $ WrongOperands "string multiplication" [String s, Int times]
  | times > fromIntegral (maxBound :: Int) =
      throwError $ WrongOperands "string multiplication" [String s, Int times]
  | otherwise =
      pure . String $ T.replicate (fromIntegral times) s
mulValues l r = exactNumericOp (*) l r

divValues :: Value -> Value -> Throws Value
divValues (Int l) (Int r) = pure $ FloatPoint (fromIntegral l / fromIntegral r)
divValues (FloatPoint l) (FloatPoint r)
  | r == 0 = throwError $ CannotDivideBy0 [FloatPoint l, FloatPoint r]
  | otherwise = pure $ FloatPoint $ l / r
divValues x y = throwError $ WrongOperands "binary numeric op" [x, y]

modValues :: Value -> Value -> Throws Value
modValues l r = numericOp (mod) (mod') l r

powValues :: Value -> Value -> Throws Value
powValues = numericOp (^) (**)

concatValues :: Value -> Value -> Throws Value
concatValues (String l) (String r) = pure $ String $ l <> r
concatValues notAString notAString2 = throwError $ WrongOperands "string concatenation" [notAString, notAString2]
