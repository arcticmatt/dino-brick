{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import Stairs

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center as C
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
handleEvent g (AppEvent Tick) = continue $ step g

-- Drawing
drawUI :: Game -> [Widget Name]
drawUI g = [w]

theMap :: AttrMap
theMap = attrMap V.defAttr
 [ (footAttr, V.white `on` V.white)
 , (stairAttr, V.white `on` V.white)
 ]

footAttr, stairAttr :: AttrName
footAttr = "footAttr"
stairAttr = "stairAttr"

ui :: Widget ()
ui = str "Hello, world!"

w :: Widget ()
w = withBorderStyle BS.ascii $
        B.border $ center $ str "Hello, world!"

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g
