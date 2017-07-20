-- {-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Dino where

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
  , _scoreMod       :: Int            -- ^ controls how often we update the score
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
  | Duck
  | Still
  deriving (Eq, Show)

makeLenses ''Game

-- Constants
gridWidth, gridHeight :: Int
gridWidth = 40
gridHeight = 20

standingDino :: Dino
standingDino = [V2 5 0, V2 5 1]

duckingDino :: Dino
duckingDino = [V2 5 0]

-- Max height the dino can reach
maxHeight :: Int
maxHeight = 5

-- Spawning min/max distances
distMin, distMax :: Int
distMin = 10
distMax = 20

-- Barrier min/max width/height
widthMin, widthMax, heightMin, heightMax :: Int
widthMin = 1
widthMax = 3
heightMin = 1
heightMax = 3

-- Update score every three frames
constScoreMod :: Int
constScoreMod = 2

-- Functions
-- | Step forward in time.
-- Increment score every tick.
step :: Game -> Game
step g = fromMaybe g $ do
  guard $ not (g^.dead)
  return $ fromMaybe (step' g) (die g)

-- | What to do if we are not dead.
step' :: Game -> Game
step' = incScore . move . spawnBarrier . deleteBarrier . adjustStanding

incScore :: Game -> Game
incScore g = case g^.scoreMod of
  0 -> g & score %~ (+1) & scoreMod %~ incAndMod
  _ -> g & scoreMod %~ incAndMod
  where incAndMod x = (x + 1) `mod` constScoreMod

-- | Possibly die if next dino position is disallowed.
die :: Game -> Maybe Game
die g = do
  guard $ die' g
  return $ g & dead .~ True

-- Check ALL barriers
-- TODO: maybe adjust to check less
die' :: Game -> Bool
die' g = let nextD = nextDino g
             nextB = nextBarriers g
          in getAny $ foldMap (Any . flip inBarriers nextB) nextD

-- | Hacky way to get the next dino. Just move it
-- and see what happens
-- TODO: is this too slow?
nextDino :: Game -> Dino
nextDino g = moveDino g^.dino

-- | Get next barriers (only consider currently existing barriers,
-- i.e. don't consider spawning a new barrier)
nextBarriers :: Game -> S.Seq Barrier
nextBarriers g = moveBarriers g^.barriers

-- | Adjust dino's standing position (i.e. is he ducking?)
-- If the direction is ducking, only make the dino stand if he is on the ground.
adjustStanding :: Game -> Game
adjustStanding g = let d = g^.dir in
  case d of
    Duck -> if isDinoBottom g then g & dino .~ duckingDino else g
    _    -> if isDinoBottom g then g & dino .~ standingDino else g

-- Moving functions
-- | Move everything on the screen
move :: Game -> Game
move = moveDino . moveBarriers

-- Dino functions
-- | Moves dino based on its current direction. If it reaches
-- the bottom of the grid, stop it. If it reaches the
-- max height, set its direction to Down.
-- Note: in order to be able to jump without missing a frame, we set the dino's
-- direction to Still ONE FRAME BEFORE it reaches the ground.
-- In order to be able to duck without missing a frame, we allow the duck's
-- direction to go from DOWN to DUCK.
moveDino :: Game -> Game
moveDino g = let d = g^.dir in
  case d of
    Up   -> if shouldStopDino d g then setDinoDir Down g else moveDino' 1 g
    Down -> if shouldStopDino d g then setDinoDir Still g else
              (let gNext = moveDino' (-1) g in
                if isDinoBottom gNext then setDinoDir Still gNext else gNext)
    Duck -> if shouldStopDino d g then g else moveDino' (-1) g
    _    -> g

-- | Moves dino up or down
moveDino' :: Int -> Game -> Game
moveDino' amt g = g & dino %~ fmap (+ V2 0 amt)

setDinoDir :: Direction -> Game -> Game
setDinoDir d g = g & dir .~ d

-- | Determines if we should stop the dino that is going in
-- the passed-in direction
shouldStopDino :: Direction -> Game -> Bool
shouldStopDino d g = case d of
  Down -> isDinoBottom g
  Duck -> isDinoBottom g
  Up   -> dinoBottom g >= maxHeight
  _    -> False

isDinoBottom :: Game -> Bool
isDinoBottom g = dinoBottom g <= 0

-- | Gets the dino's bottom ypos
dinoBottom :: Game -> Int
dinoBottom g =
  let d = g^.dino
      (V2 _ y) = head d -- note: no error checking here (TODO?)
  in y

-- Barrier functions
-- | Move all the barriers
moveBarriers :: Game -> Game
moveBarriers g = g & barriers %~ fmap moveBarrier

-- | Move single barrier left
moveBarrier :: Barrier -> Barrier
moveBarrier = fmap (+ V2 (-1) 0)

-- | Delete barrier if it has gone off the left side
deleteBarrier :: Game -> Game
deleteBarrier g =
  case viewl $ g^.barriers of
    EmptyL  -> g
    a :< as -> let x = getBarrierRightmost a in
                 (if x <= 0 then g & barriers .~ as else g)

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
                if (gridWidth - x) > r then addRandomBarrier g & rands .~ rs else g

getBarrierLeftmost :: Barrier -> Int
getBarrierLeftmost [] = error "empty barrier"
getBarrierLeftmost (V2 x _:_) = x

getBarrierRightmost :: Barrier -> Int
getBarrierRightmost [] = error "empty barrier"
getBarrierRightmost b = let (V2 x _) = last b in x

-- | Add random barrier (constrained random width/height)
-- Note: In (V4 w x y z), w x is bottom left, y z is top right
addRandomBarrier :: Game -> Game
addRandomBarrier g =
  let (V2 w h:rest) = g^.dimns
      newBarrier = [V2 (gridWidth + a) (0 + b)
                    | a <- [0..w-1], b <- [0..h-1]]
  in g & barriers %~ (|> newBarrier) & dimns .~ rest

-- | Checks to see if the passed-in coordinate is in any
-- of the barriers
inBarriers :: Coord -> S.Seq Barrier -> Bool
inBarriers c bs = getAny $ foldMap (Any . inBarrier c) bs

-- | Checks to see if the passed-in coordinate is in the
-- passed-in barriers
inBarrier :: Coord -> Barrier -> Bool
inBarrier c b = c `elem` b

-- Initialization functions
-- | Initialize a game with random stair location
initGame :: IO Game
initGame = do
  randomDists <- randomRs (distMin, distMax) <$> newStdGen
  dimensions  <- randomRs (V2 widthMin heightMin, V2 widthMax heightMax) <$> newStdGen
  let g = Game { _dino = standingDino
               , _dir = Still
               , _barriers = S.empty
               , _rands = randomDists
               , _dimns = dimensions
               , _dead = False
               , _scoreMod = 0
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
