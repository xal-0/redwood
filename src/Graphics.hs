module Graphics where

import Control.Monad.Except
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Interpreter
import Numeric
import Runtime
import System.Exit

-- | Extra state for the graphical interpreter.  The extra builtins
-- can read/write these variables, and they are used to communicate
-- things like the state of the current frame, helds keys, etc.
data State = State
  { drawnPictures :: IORef Picture,
    heldKeys :: IORef (S.Set Key),
    bitmaps :: IORef (M.Map String Picture)
  }

-- | Run the source code at the given path within the graphical
-- environment.
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

-- | Make a fresh state of the graphical interpreter.
newState :: IO State
newState = do
  pict <- newIORef Blank
  keys <- newIORef S.empty
  sprites <- newIORef M.empty
  pure (State pict keys sprites)

-- | Read the Picture of the current frame to be displayed using
-- gloss.
frame :: State -> () -> IO Picture
frame state () = readIORef (drawnPictures state)

-- | Change the key variables upon user input.
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

-- | These are the additional bindings that are aviaable to programs
-- run in the graphical interpreter.  They're provided with the State
-- so that they can read/read the extra IORefs.
graphicsBuiltins :: [(String, State -> Prim)]
graphicsBuiltins =
  [ ("key", keyBuiltin),
    ("circle", circleBuiltin),
    ("line", lineBuiltin),
    ("rect", rectBuiltin),
    ("sprite", spriteBuiltin),
    ("text", textBuiltin)
  ]

-- | Helper function that draws the given picture over the current
-- frame.
addToPicture :: State -> Picture -> Interpret Value
addToPicture state p = do
  liftIO (modifyIORef (drawnPictures state) f)
  pure ValueNull
  where
    f ps = Pictures [ps, p]

-- | Parse a hex colour like "aabbcc".
hexToColor :: String -> Interpret Color
hexToColor [r1, r2, g1, g2, b1, b2] =
  makeColor <$> comp r1 r2 <*> comp g1 g2 <*> comp b1 b2 <*> pure 1
  where
    comp :: Char -> Char -> Interpret Float
    comp x y = case readHex [x, y] of
      [(n, _)] -> pure (fromIntegral (n :: Int))
      _ -> throwError (ErrMisc "invalid colour")
hexToColor _ = throwError (ErrMisc "invalid colour")

-- | Draw a circle with the given radius, colour, and position.
circleBuiltin :: State -> Prim
circleBuiltin state [ValueString col, ValueNumber r, ValueNumber x, ValueNumber y] = do
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

-- | Draw a line of the given colour between two points.
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

-- | Draw a sprite, loaded from BMP in the current directory.  Will
-- only the bitmap once, future calls to sprite with the same filename
-- will not re-load it.
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

-- | Draw a rectangle with the given colour, at the given posiiton,
-- with the given width and height.
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

-- | Draw seome text with the given colour, size, and position.
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

-- | Check t osee if the given key is currently pressed.  Supports
-- regular keys, as well as the space bar and arrow keys.
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
