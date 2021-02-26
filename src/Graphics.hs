
module Graphics where

import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
import qualified Data.Set as S

-- Contains all of the information kept about the vairables and keys pressed
data WorldState = WorldState Env Program Sprites Keys

type Sprites = (M.Map String Sprite)
type Sprite = Picture int int

type Keys = (S.Set EventKey) 

main :: IO ()
main = playIO (InWindow "my program" (800, 600) (10, 10))
        black 10
        10
        frame inputs update

-- draws a Picture to be diplayed using the world state
frame :: WorldState -> IO Picture
frame (env prog sprites keys) = Circle 10

-- changes the key variables upon user input
inputs :: Event -> WorldState -> IO WorldState
inputs (EventKey keyName Down _ _) (env prog sprites keys) = pure (WorldState env prog sprites (insert keyName keys))
inputs (EventKey keyName Up _ _) (env prog sprites keys) = pure (WorldState env prog sprites (delete keyName keys))

-- calls the "draw" function in the users code and updates the worldstate accordingly
update :: Float -> WorldState -> IO WorldState
update time world = pure world