module Main where

import Graphics
import Interpreter
import System.Environment

-- | Parse the command line arguments, and run the source code
-- provided as the last argument.  If the "-c" flag is provided, do
-- not run the graphical environment.
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
