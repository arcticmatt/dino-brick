-- {-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Dino where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..))
import System.Random (Random(..), randomRs, newStdGen)
import qualified Data.Sequence as S
import Data.Sequence(ViewR(..), ViewL(..), viewr, viewl, (|>))
import Data.Monoid (Any(..), getAny)

-- Types
data Game = Game
  { _dino           :: Dino           -- ^ dinosaur
  , _dir            :: Direction      -- ^ direction of left foot
  , _barriers       :: S.Seq Barrier  -- ^ sequence of barriers on screen
  , _rands          :: [Int]          -- ^ random numbers for spawning
  , _dimns          :: [Dimension]    -- ^ random barrier dimensions
  , _dead           :: Bool           -- ^ game over flag
  , _score          :: Int            -- ^ score
  } deriving (Show)

type Coord = V2 Int
type Dino = [Coord] -- we'll represent the dino by 1 or 2 points
type Barrier = [Coord] -- barriers will be 1-6 points around
-- TODO: probably not worth it to use sequences here
type Dimension = V2 Int

data Direction =
    Up
  | Down
  | Still
  deriving (Eq, Show)

makeLenses ''Game

-- Constants
gridWidth, gridHeight :: Int
gridWidth = 40
gridHeight = 20

initialDino :: Dino
initialDino = [V2 5 0, V2 5 1]

-- Max height the dino can reach
maxHeight :: Int
maxHeight = 5

-- Spawning min/max distances
distMin, distMax :: Int
distMin = 1
distMax = 30

-- Barrier min/max width/height
widthMin, widthMax, heightMin, heightMax :: Int
widthMin = 1
widthMax = 3
heightMin = 1
heightMax = 3

-- Functions
-- | Step forward in time.
-- TODO: gameover
step :: Game -> Game
step g = fromMaybe g $ do
  guard $ not (g^.dead)
  return . fromMaybe (move . spawnBarrier . deleteBarrier $ g) $ die g

-- | Possibly die if next dino position is disallowed.
die :: Game -> Maybe Game
die g = do
  guard $ die' g
  return $ g & dead .~ True

-- Only check leftmost barrier
die' :: Game -> Bool
die' g = let nextD = nextDino g
             nextB = nextBarrier g
         in case nextB of
           Nothing -> False
           Just b  -> getAny $ foldMap (Any . (flip inBarrier) b) nextD

-- | Hacky way to get the next dino. Just move it
-- and see what happens
-- TODO: is this too slow?
nextDino :: Game -> Dino
nextDino g = (moveDino g)^.dino

-- | Hacky way to get the next (leftmost) barrier.
nextBarrier :: Game -> Maybe Barrier
nextBarrier g = case viewl $ (moveBarriers g)^.barriers of
  EmptyL -> Nothing
  b :< _ -> Just b

-- Moving functions
-- | Move everything on the screen
move :: Game -> Game
move = moveDino . moveBarriers

-- Dino functions
-- | Moves dino based on its current direction. If it reaches
-- the bottom of the grid, stop it. If it reaches the
-- max height, set its direction to Down.
moveDino :: Game -> Game
moveDino g = let d = g^.dir in
  case d of
    Up   -> case shouldStopDino d g of
              True  -> setDinoDir Down g
              False -> moveDino' 1 g
    Down -> case shouldStopDino d g of
              True  -> setDinoDir Still g
              False -> moveDino' (-1) g
    _    -> g

-- | Moves dino up or down
moveDino' :: Int -> Game -> Game
moveDino' amt g = g & dino %~ (fmap (+(V2 0 amt)))

setDinoDir :: Direction -> Game -> Game
setDinoDir d g = g & dir .~ d

-- | Determines if we should stop the dino that is going in
-- the passed-in direction
shouldStopDino :: Direction -> Game -> Bool
shouldStopDino d g = case d of
  Down -> dinoBottom g <= 0
  Up   -> dinoBottom g >= maxHeight
  _    -> False

-- | Gets the dino's bottom ypos
dinoBottom :: Game -> Int
dinoBottom g =
  let d = g^.dino
      (V2 _ y) = head d -- note: no error checking here (TODO?)
  in y

-- Barrier functions
-- | Move all the barriers
moveBarriers :: Game -> Game
moveBarriers g = g & barriers %~ (fmap moveBarrier)

-- | Move single barrier left
moveBarrier :: Barrier -> Barrier
moveBarrier = fmap (+ (V2 (-1) 0))

-- | Delete barrier if it has gone off the left side
deleteBarrier :: Game -> Game
deleteBarrier g =
  case viewl $ g^.barriers of
    EmptyL  -> g
    a :< as -> let x = getBarrierRightmost a in
                 case x <= 0 of
                   True  -> g & barriers .~ as  -- remove
                   False -> g                  -- do nothing


-- | Spawn new barrier (if necessary).
-- We spawn barriers somewhat randomly, as follows.
-- 1) Pick random number in range (generated initially b/c we need IO)
-- 2) If last barrier (closest to right side of screen) is
-- beyond that random number, we add another barrier to the sequence.
spawnBarrier :: Game -> Game
spawnBarrier g =
  let (r:rs) = g^.rands
  in case viewr $ g^.barriers of
    EmptyR -> addRandomBarrier g
    _ :> a -> let x = getBarrierLeftmost a in
                case (gridWidth - x) > r of
                  True  -> addRandomBarrier g & rands .~ rs
                  False -> g

getBarrierLeftmost :: Barrier -> Int
getBarrierLeftmost [] = error "empty barrier"
getBarrierLeftmost ((V2 x _):_) = x

getBarrierRightmost :: Barrier -> Int
getBarrierRightmost [] = error "empty barrier"
getBarrierRightmost b = let (V2 x _) = last b in x

-- | Add random barrier (constrained random width/height)
-- Note: In (V4 w x y z), w x is bottom left, y z is top right
addRandomBarrier :: Game -> Game
addRandomBarrier g =
  let ((V2 w h):rest) = g^.dimns
      newBarrier = [V2 (gridWidth + a) (0 + b)
                    | a <- [0..w-1], b <- [0..h-1]]
  in g & barriers %~ (|> newBarrier) & dimns .~ rest

-- | Checks to see if the passed-in coordinate is in any
-- of the barriers
inBarriers :: Coord -> Game -> Bool
inBarriers c g = getAny $ foldMap (Any . inBarrier c) (g^.barriers)

-- | Checks to see if the passed-in coordinate is in the
-- passed-in barriers
inBarrier :: Coord -> Barrier -> Bool
inBarrier c b = c `elem` b

-- Initialization functions
-- | Initialize a game with random stair location
initGame :: IO Game
initGame = do
  randoms <- randomRs (distMin, distMax) <$> newStdGen
  dimensions <- randomRs (V2 widthMin heightMin, V2 widthMax heightMax) <$> newStdGen
  let g = Game { _dino = initialDino
               , _dir = Still
               , _barriers = S.empty
               , _rands = randoms
               , _dimns = dimensions
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
