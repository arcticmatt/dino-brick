{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Dino
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

data Cell = Dino | Barrier | Empty

-- Constants
minFrameRate :: Int
minFrameRate = 30000

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
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ handleUp g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ handleDown g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'r') [])) =
  liftIO (writeIORef counter 0 >> initGame) >>= continue
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

-- Mostly copied from snake example
drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " ||| dino ||| ")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [gridHeight - 1,gridHeight - 2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..gridWidth - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` g^.dino           = Dino
      | inBarriers c (g^.barriers) = Barrier
      | otherwise                  = Empty

drawCell :: Cell -> Widget Name
drawCell Dino    = withAttr dinoAttr cw
drawCell Barrier = withAttr barrierAttr cw
drawCell Empty   = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
 [ (dinoAttr, V.blue `on` V.blue)
 , (barrierAttr, V.green `on` V.green)
 -- , (emptyAttr, V.white `on` V.white) -- TODO: change/remove
 , (gameOverAttr, fg V.red `V.withStyle` V.bold)
 ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

dinoAttr, barrierAttr, emptyAttr :: AttrName
dinoAttr = "dinoAttr"
barrierAttr = "barrierAttr"
emptyAttr = "emptyAttr"

counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)

playGame :: IO Game
playGame = do
  chan <- newBChan 10
  forkIO $ forever $ do
    modifyIORef counter (+1)
    c' <- readIORef counter
    writeBChan chan Tick
    threadDelay (max (70000 - c' * 10) 30000)
  g <- initGame
  customMain (V.mkVty V.defaultConfig) (Just chan) app g
