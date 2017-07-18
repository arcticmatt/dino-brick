module UI where

import Stairs

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import qualified Graphics.Vty as V

-- Types
-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

-- App definition
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- Handling events
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent = undefined

-- Drawing
drawUI :: Game -> [Widget Name]
drawUI g = undefined

theMap :: AttrMap
theMap = undefined

ui :: Widget ()
ui = str "Hello, world!"

w :: Widget ()
w = withBorderStyle Brick.Widgets.Border.Style.ascii $
        Brick.Widgets.Border.border $ center $ str "Hello, world!"

main :: IO ()
main = simpleMain w
