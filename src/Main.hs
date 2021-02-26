module Main where

import Interpreter
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      interp <- makeInterpreter []
      _ <- evalSource interp filename
      pure ()
    _ -> putStrLn "provide the file to execute as an argument"
