{-# LANGUAGE RankNTypes #-}

module Value where

import Data.Fixed
import Data.Text (Text, pack)
import Text.Printf

type MzInt = Integer
type MzFloat = Double

data Value = Int MzInt | FloatPoint MzFloat | String Text | CharLit Char | Boolean Bool

showValue :: Value -> String
showValue (Int x) = show x
showValue (FloatPoint x) = show x
showValue (String s) = show s
showValue (CharLit c) = show c
showValue (Boolean b) = printf "%s" (if b then "true" else "false")

unpackNum :: Value -> Bool
unpackNum (Boolean x) = x
unpackNum _ = False

numericOp :: (MzInt -> MzInt -> MzInt) -> (MzFloat -> MzFloat -> MzFloat) -> Value -> Value -> Value
numericOp op _ (Int l) (Int r) = Int $ l `op` r
numericOp _ op (FloatPoint l) (FloatPoint r) = FloatPoint $ l `op` r
numericOp _ _ notAInt notAInt2 = String $ pack ("expected int, found " <> show notAInt <> show notAInt2)

unaryNumericOp :: (MzInt -> MzInt) -> (MzFloat -> MzFloat) -> Value -> Value
unaryNumericOp op _ (Int i) = Int $ op i
unaryNumericOp _ op (FloatPoint f) = FloatPoint $ op f
unaryNumericOp _ _ notNum = String $ pack ("expected num, found " <> show notNum)

exactNumericOp :: (forall a. (Num a) => a -> a -> a) -> Value -> Value -> Value
exactNumericOp op = numericOp op op

exactUnaryOp :: (forall a. (Num a) => a -> a) -> Value -> Value
exactUnaryOp op = unaryNumericOp op op

negateValue :: Value -> Value
negateValue = exactUnaryOp (\x -> -x)

notValue :: Value -> Value
notValue (Boolean x) = Boolean $ not x
notValue _ = Boolean False

addValues :: Value -> Value -> Value
addValues = exactNumericOp (+)

subValues :: Value -> Value -> Value
subValues = exactNumericOp (-)

mulValues :: Value -> Value -> Value
mulValues = exactNumericOp (*)

divValues :: Value -> Value -> Value
divValues (Int l) (Int r) = FloatPoint (fromIntegral l / fromIntegral r)
divValues (FloatPoint l) (FloatPoint r) = FloatPoint $ l / r
divValues x y = String $ pack ("expected numbers, found " <> show x <> " " <> show y)

modValues :: Value -> Value -> Value
modValues l r = numericOp (mod) (mod') l r

powValues :: Value -> Value -> Value
powValues = numericOp (^) (**)

concatValues :: Value -> Value -> Value
concatValues (String l) (String r) = String $ l <> r
concatValues notAString notAString2 = String $ pack ("expected strings found: " <> show notAString <> " " <> show notAString2)

instance Show Value where
  show = showValue
