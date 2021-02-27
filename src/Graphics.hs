module Graphics where

import Control.Monad.Except
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Interpreter
import Runtime
import System.Exit
import Numeric

data State = State
  { drawnPictures :: (IORef Picture),
    heldKeys :: (IORef (S.Set Key)),
    bitmaps :: (IORef (M.Map String Picture))
  }

graphicsRun :: FilePath -> IO ()
graphicsRun path = do
  state <- newState
  int <- makeInterpreter (fmap (\(k, b) -> (k, b state)) graphicsBuiltins)
  success <- evalSource int path
  if not success
    then pure ()
    else
      playIO
        (InWindow "Redwood" (400, 400) (10, 10))
        black
        60
        ()
        (frame state)
        (inputs state)
        (update state int)

newState :: IO State
newState = do
  pict <- newIORef Blank
  keys <- newIORef S.empty
  sprites <- newIORef M.empty
  pure (State pict keys sprites)

-- | Draws a Picture to be diplayed using the world state
frame :: State -> () -> IO Picture
frame state () = readIORef (drawnPictures state)

-- | Change the key variables upon user input
inputs :: State -> Event -> () -> IO ()
inputs state (EventKey key Down _ _) () = do
  modifyIORef (heldKeys state) (S.insert key)
  pure ()
inputs state (EventKey key Up _ _) () = do
  modifyIORef (heldKeys state) (S.delete key)
  pure ()
inputs _ _ () = pure ()

-- | Calls the "update" function in the user's code.
update :: State -> Interpreter -> Float -> () -> IO ()
update state int _ () = do
  writeIORef (drawnPictures state) Blank
  result <- evalCall int "update"
  case result of
    Nothing -> exitFailure
    Just _ -> pure ()

graphicsBuiltins :: [(String, State -> Prim)]
graphicsBuiltins =
  [ ("key", keyBuiltin),
    ("circle", circleBuiltin),
    ("line", lineBuiltin),
    ("rect", rectBuiltin),
    ("sprite", spriteBuiltin),
    ("text", textBuiltin)
  ]

addToPicture :: State -> Picture -> Interpret Value
addToPicture state p = do
  liftIO (modifyIORef (drawnPictures state) f)
  pure ValueNull
  where
    f (Pictures l) = Pictures (p : l)
    f ps = Pictures [p, ps]

hexToColor :: String -> Interpret Color
hexToColor [r1, r2, g1, g2, b1, b2] =
  makeColor <$> comp r1 r2 <*> comp g1 g2 <*> comp b1 b2 <*> pure 0
  where
    comp :: Char -> Char -> Interpret Float
    comp x y = case readHex [x, y] of
      [(n, _)] -> pure (fromIntegral (n :: Int))
      _ -> throwError (ErrMisc "invalid colour")
hexToColor _ = throwError (ErrMisc "invalid colour")

circleBuiltin :: State -> Prim
circleBuiltin state [ValueNumber r, ValueString col, ValueNumber x, ValueNumber y] = do
  col' <- hexToColor col
  addToPicture
    state
    ( Color
        col'
        ( Translate
            (realToFrac x)
            (realToFrac y)
            (Circle (realToFrac r))
        )
    )
circleBuiltin _ _ = throwError (ErrMisc "circle expects a radius, colour, x, and y")

lineBuiltin :: State -> Prim
lineBuiltin state [ValueString col, ValueNumber x1, ValueNumber y1, ValueNumber x2, ValueNumber y2] = do
  col' <- hexToColor col
  addToPicture
    state
    ( Color
        col'
        ( Line
            [ (realToFrac x1, realToFrac y1),
              (realToFrac x2, realToFrac y2)
            ]
        )
    )
lineBuiltin _ _ = throwError (ErrMisc "line takes a colour, x1, y1, x2, and y2")

spriteBuiltin :: State -> Prim
spriteBuiltin state [ValueString filename, ValueNumber x, ValueNumber y] = do
  bms <- liftIO (readIORef (bitmaps state))
  case M.lookup filename bms of
    Just bm -> draw bm
    Nothing -> do
      bm <- liftIO (loadBMP filename)
      liftIO (writeIORef (bitmaps state) (M.insert filename bm bms))
      draw bm
  where
    draw bm = addToPicture state (Translate (realToFrac x) (realToFrac y) bm)
spriteBuiltin _ _ = throwError (ErrMisc "sprite takes a sprite filename, x, and y")

rectBuiltin :: State -> Prim
rectBuiltin state [ValueString col, ValueNumber x, ValueNumber y, ValueNumber w, ValueNumber h] = do
  col' <- hexToColor col
  addToPicture
    state
    ( Color
        col'
        ( Polygon
            [ (realToFrac x, realToFrac y),
              (realToFrac x, realToFrac (y + h)),
              (realToFrac (x + w), realToFrac (y + h)),
              (realToFrac (x + w), realToFrac y)
            ]
        )
    )
rectBuiltin _ _ = throwError (ErrMisc "rect takes a colour x, y, w, and h")

textBuiltin :: State -> Prim
textBuiltin state [ValueString col, ValueNumber size, ValueNumber x, ValueNumber y, ValueString txt] = do
  col' <- hexToColor col
  addToPicture
    state
    ( Translate
        (realToFrac x)
        (realToFrac y)
        ( Scale
            (realToFrac size)
            (realToFrac size)
            ( Color
                col'
                (Text txt)
            )
        )
    )
textBuiltin _ _ = throwError (ErrMisc "text takes a colour, size, x, y, and a string")

keyBuiltin :: State -> Prim
keyBuiltin state [ValueString k] = do
  key <- case k of
    "space" -> pure (SpecialKey KeySpace)
    "up" -> pure (SpecialKey KeyUp)
    "down" -> pure (SpecialKey KeyDown)
    "left" -> pure (SpecialKey KeyLeft)
    "right" -> pure (SpecialKey KeyRight)
    [c] -> pure (Char c)
    _ -> throwError (ErrMisc "key expects one string argument")
  keys' <- liftIO (readIORef (heldKeys state))
  pure (ValueBool (key `S.member` keys'))
keyBuiltin _ _ = throwError (ErrMisc "key expects one string argument")
