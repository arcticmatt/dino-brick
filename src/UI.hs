{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
-- import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import Stairs
import Controls

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Lens.Micro ((^.))

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

data Cell = FootCell | StairCell | EmptyCell

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
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ changeRight g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ changeLeft g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ changeRight g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ changeLeft g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _ = continue g

-- Drawing
drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

-- Copied from snake example
drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

-- Copied from snake example
drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

-- Copied from snake example
drawGameOver :: Bool -> Widget Name
drawGameOver isDead =
  if isDead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "stairs")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [gridHeight - 1,gridHeight - 2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..gridWidth - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | isFeet c g   = FootCell -- order kinda matters (if we want diff colors)
      | isStairs c g = StairCell
      | otherwise    = EmptyCell

drawCell :: Cell -> Widget Name
drawCell FootCell = withAttr footAttr cw
drawCell StairCell = withAttr stairAttr cw
drawCell EmptyCell = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
 [ (footAttr, V.white `on` V.white)
 , (stairAttr, V.green `on` V.green)
 , (emptyAttr, V.blue `on` V.blue) -- TODO: change/remove
 , (gameOverAttr, fg V.red `V.withStyle` V.bold)
 ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

footAttr, stairAttr, emptyAttr :: AttrName
footAttr = "footAttr"
stairAttr = "stairAttr"
emptyAttr = "emptyAttr"

-- ui :: Widget ()
-- ui = str "Hello, world!"
--
-- w :: Widget ()
-- w = withBorderStyle BS.ascii $
--         B.border $ center $ str "Hello, world!"

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 40000
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g
