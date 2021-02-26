
module Main where

import Graphics.Gloss.Interface.IO.Game

type WorldState = Double

main :: IO ()
main    = playIO (InWindow "machina" (800, 600) (10, 10))
                black 10
                10
                frame inputs update

-- draws a Picture to be diplayed using the world state
frame :: WorldState -> IO Picture
frame time = pure (Circle 100)

-- changes the key variables upon user input
inputs :: Event -> WorldState -> IO WorldState
inputs (EventKey (SpecialKey KeyLeft) Down _ _) world = pure world 

-- calls the "draw" function in the users code and updates the worldstate accordingly
update :: Float -> WorldState -> IO WorldState
update time world = pure world