module Repl where

import Data.Text as T
import Eval
import Parser
import System.Console.ANSI
import System.IO
import Text.Megaparsec (errorBundlePretty)

flushS :: String -> IO ()
flushS s = putStr s >> hFlush stdout

readPrompt :: IO String
readPrompt = do
  setSGR [SetColor Foreground Dull Green]
  flushS ":mz:"
  setSGR [Reset]
  flushS "> " >> getLine

evalString :: String -> IO ()
evalString expr = do
  case runParserString (T.pack expr) of
    Left err -> putStrLn (errorBundlePretty err)
    Right ast -> print $ eval ast

until_ :: (Monad m) => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = until_ (== ":q") (readPrompt) (evalString)
