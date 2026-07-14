module Main (main) where

import Repl
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalString (args !! 0) >>= putStrLn
    _ -> putStrLn "Wrong Argument Numbers"
