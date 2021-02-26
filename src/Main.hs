module Main where

import Interpreter
import System.Environment
import Graphics

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-c", filename] -> do
      interp <- makeInterpreter []
      _ <- evalSource interp filename
      pure ()
    [filename] -> do
      graphicsRun filename
    _ -> putStrLn "usage: redwood [-c] file.rw"
