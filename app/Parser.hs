{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Ast
import Control.Monad.Combinators.Expr
import Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Error = Void
type Input = Text

type Parser = Parsec Error Input

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = skipMany space1

parseInteger :: Parser Expr
parseInteger = Integer <$> Parser.lexeme (L.decimal <|> L.binary <|> L.octal <|> L.hexadecimal)

parseFloat :: Parser Expr
parseFloat = Float <$> Parser.lexeme L.float

parseString :: Parser Expr
parseString = String . T.pack <$> between (char '"') (char '"') (many anySingle)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

parseIdentifier :: Parser Expr
parseIdentifier = do
  start <- letterChar <|> char '_'
  rest <- many (letterChar <|> digitChar <|> char '_')
  let id = start : rest
  pure $ Id $ T.pack id

parseTerm :: Parser Expr
parseTerm = choice [parens parseExpr, parseString, parseInteger, parseFloat, parseIdentifier]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ Parser.symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ Parser.symbol name)
postfix name f = Postfix (f <$ Parser.symbol name)

operatorTable :: [[Operator Parser Expr]]
operatorTable = [[prefix "-" Negation]]

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable
