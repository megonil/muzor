{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Ast
import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text
