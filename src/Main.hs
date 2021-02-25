module Main where

import Interpreter
import Parser
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  input <- getContents
  case parse (stmts <* eof) "<repl>" input of
    Left err -> putStrLn (errorBundlePretty err)
    Right prog -> testInterpret prog >>= print
