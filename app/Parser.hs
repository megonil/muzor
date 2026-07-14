{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Ast
import Control.Monad.Combinators.Expr
import Data.Scientific
import Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Error = Void
type Input = Text

type Parser = Parsec Error Input

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = skipMany space1

parseNumber :: Parser Expr
parseNumber = do
  x <- lexeme L.scientific
  case floatingOrInteger x of
    Left d -> pure $ FloatLit d
    Right i -> pure $ Integer i

parseString :: Parser Expr
parseString =
  StringLit . T.pack
    <$> lexeme (char '"' *> manyTill L.charLiteral (char '\"'))

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

parseIdentifier :: Parser Expr
parseIdentifier = do
  start <- letterChar <|> char '_'
  rest <- many (letterChar <|> digitChar <|> char '_')
  let id = start : rest
  pure $ Id $ T.pack id

parseChar :: Parser Expr
parseChar = Char <$> lexeme (between (char '\'') (char '\'') L.charLiteral)

parseTerm :: Parser Expr
parseTerm =
  choice
    [ parens parseExpr
    , parseString
    , parseChar
    , parseNumber
    , parseIdentifier
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

binaryR :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryR name f = InfixR (f <$ symbol name)
prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" Negation, prefix "!" Not]
  , [binary "^" Pow]
  , [binary "*" Mul, binary "/" Div]
  , [binary "+" Add, binary "-" Sub, binary "<>" Concat]
  ]

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable

runParserString :: Text -> Either (ParseErrorBundle Text Void) Expr
runParserString s = runParser parseExpr "source" s
