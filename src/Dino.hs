-- {-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Dino where

import Lens.Micro.TH (makeLenses)
import Lens.Micro.Type
import Lens.Micro ((&), (.~), (%~), (^.), mapped)
import Linear.V2 (V2(..), _y)
import System.Random (Random(..), newStdGen)
import qualified Data.Sequence as S
import Data.Ix (inRange)
import Data.Monoid (Any(..), getAny)

-- Types
data Game = Game
  { _dino           :: Dino           -- ^ dinosaur
  , _dir            :: Direction      -- ^ direction of left foot
  , _barriers       :: S.Seq Barrier  -- ^ sequence of barriers on screen
  , _dead           :: Bool           -- ^ game over flag
  , _score          :: Int            -- ^ score
  } deriving (Show)

type Coord = V2 Int
type Size = Int
type Dino = [Coord] -- we'll represent the dino by 1 or 2 points
type Barrier = [Coord] -- barriers will be 1-3 points
-- TODO: probably not worth it to use sequences here

data Direction =
    Up
  | Down
  | Still
  deriving (Eq, Show)

makeLenses ''Game

-- Constants
gridWidth, gridHeight :: Size
gridWidth = 35
gridHeight = 35

-- Max height the dino can reach
maxHeight :: Int
maxHeight = 5

-- Functions
-- | Step forward in time.
-- TODO:
step :: Game -> Game
step = id

-- Moving functions
-- | Move everything on the screen
move :: Game -> Game
move = undefined
-- move = undefined

-- | See if we should spawn another barrier
shouldSpawn :: Game -> Bool
shouldSpawn g = undefined

-- Initialization functions
-- | Initialize a game with random stair location
initGame :: IO Game
initGame = do
  let g = Game { _dino = []
               , _dir = Still
               , _barriers = S.empty
               , _dead = False
               , _score = 0 }
  return g

instance Random a => Random (V2 a) where
  randomR (V2 x1 y1, V2 x2 y2) g =
    let (x, g')  = randomR (x1, x2) g
        (y, g'') = randomR (y1, y2) g'
     in (V2 x y, g'')
  random g =
    let (x, g')  = random g
        (y, g'') = random g'
     in (V2 x y, g'')
