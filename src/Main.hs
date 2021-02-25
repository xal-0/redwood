module Main where

import Interpreter
import Parser
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      fileContents <- readFile filename
      case parse (stmts <* eof) filename fileContents of
        Left err -> putStrLn (errorBundlePretty err)
        Right prog -> do
          result <- testInterpret prog
          case result of
            Left e -> print e
            Right _ -> pure ()
    _ -> putStrLn "provide the file to execute as an argument"
