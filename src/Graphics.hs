module Graphics where

import Control.Monad.Except
import Data.IORef
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game
import Interpreter
import Runtime
import System.Exit

graphicsRun :: FilePath -> IO ()
graphicsRun path = do
  picture <- newIORef Blank
  keys <- newIORef S.empty
  int <- makeInterpreter (fmap (\(k, b) -> (k, b picture keys)) graphicsBuiltins)
  success <- evalSource int path
  if not success
    then pure ()
    else
      playIO
        (InWindow "Redwood" (800, 600) (10, 10))
        white
        30
        ()
        (frame picture)
        (inputs keys)
        (update picture int)

-- | Draws a Picture to be diplayed using the world state
frame :: IORef Picture -> () -> IO Picture
frame picture () = readIORef picture

-- | Change the key variables upon user input
inputs :: IORef (S.Set Key) -> Event -> () -> IO ()
inputs s (EventKey key Down _ _) () = do
  modifyIORef s (S.insert key)
  pure ()
inputs s (EventKey key Up _ _) () = do
  modifyIORef s (S.delete key)
  pure ()
inputs _ _ () = pure ()

-- | Calls the "update" function in the user's code.
update :: IORef Picture -> Interpreter -> Float -> () -> IO ()
update picture int _ state = do
  writeIORef picture Blank
  result <- evalCall int "update"
  case result of
    Nothing -> exitFailure
    Just _ -> pure state

graphicsBuiltins :: [(String, IORef Picture -> IORef (S.Set Key) -> Prim)]
graphicsBuiltins =
  [ ("key", keyBuiltin),
    ("circle", circleBuiltin)
  ]

circleBuiltin :: IORef Picture -> IORef (S.Set Key) -> Prim
circleBuiltin picture _ [ValueNumber r, ValueNumber x, ValueNumber y] = do
  liftIO
    ( modifyIORef
        picture
        ( \p ->
            Pictures
              [ Translate
                  (realToFrac x)
                  (realToFrac y)
                  (Circle (realToFrac r)),
                p
              ]
        )
    )
  pure ValueNull
circleBuiltin _ _ _ = throwError (ErrMisc "circle expects a radius, x, and y")
>>>>>>> ffa30f249fb397bd0261adf9f5829b8e5ae5d7fa

keyBuiltin :: IORef Picture -> IORef (S.Set Key) -> Prim
keyBuiltin _ keys [ValueString k] = do
  key <- case k of
    "space" -> pure (SpecialKey KeySpace)
    "up" -> pure (SpecialKey KeyUp)
    "down" -> pure (SpecialKey KeyDown)
    "left" -> pure (SpecialKey KeyLeft)
    "right" -> pure (SpecialKey KeyRight)
    [c] -> pure (Char c)
    _ -> throwError (ErrMisc "key expects one string argument")
  keys' <- liftIO (readIORef keys)
  pure (ValueBool (key `S.member` keys'))
keyBuiltin _ _ _ = throwError (ErrMisc "key expects one string argument")
