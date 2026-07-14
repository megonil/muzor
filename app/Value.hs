{-# LANGUAGE RankNTypes #-}

module Value where

import Data.Text (Text)
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

instance Show Value where
  show = showValue
