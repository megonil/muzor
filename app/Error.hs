module Error where

import Control.Monad.Except
import System.Console.ANSI
import Value

data MzError = TypeMismatch String Value | WrongOperands String [Value] | CannotDivideBy0 [Value]

unwordsList :: (Show a) => [a] -> String
unwordsList = unwords . map show

showMzError :: MzError -> String
showMzError (TypeMismatch expected found) = "Expected " <> expected <> " got " <> show found
showMzError (WrongOperands to operands) = "Wrong operands to " <> to <> " operator: " <> unwordsList operands
showMzError (CannotDivideBy0 found) = "Cannot divide by 0 in " <> show (found !! 0) <> "/" <> show (found !! 1)

instance Show MzError where
  show e = setSGRCode [SetColor Foreground Vivid Red] <> showMzError e <> setSGRCode [Reset]

type Throws = Either MzError

extractValue :: Throws b -> b
extractValue (Right v) = v

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractTrap :: Throws String -> String
extractTrap = extractValue . trapError
