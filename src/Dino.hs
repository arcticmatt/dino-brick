{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Dino where

import qualified Data.Map as M
import Data.Ratio ((%))
import Control.Monad.Random
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
  , _dimns          :: [Dimension]    -- ^ random barrier dimensions
  , _positions      :: [Position]     -- ^ random barrier positions
  , _level          :: Difficulty     -- ^ game's difficulty level
  , _diffMap        :: DifficultyMap  -- ^ game's difficulty map
  , _dead           :: Bool           -- ^ game over flag
  , _paused         :: Bool           -- ^ paused flag
  , _scoreMod       :: Int            -- ^ controls how often we update the score
  , _score          :: Score          -- ^ score
  , _highscore      :: Score          -- ^ highscore of current sesh
  } deriving (Show)

type Score = Int
type Coord = V2 Int
type Dino = [Coord] -- we'll represent the dino by 1 or 2 points
type Barrier = [Coord] -- barriers will be 1-6 points around
-- TODO: probably not worth it to use sequences here
type Dimension = V2 Int
data DifficultyMap = DifficultyMap
  { _d0 :: DiffMod
  , _d1 :: DiffMod
  , _d2 :: DiffMod
  , _d3 :: DiffMod
  , _d4 :: DiffMod
  , _d5 :: DiffMod
  , _d6 :: DiffMod
  } deriving (Eq, Show)

data Direction =
    Up
  | Down
  | Duck
  | Still
  deriving (Eq, Show)

data Difficulty =
    D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  deriving (Eq, Ord, Show, Enum)

data Position = Ground | Sky deriving (Eq, Show)

-- These modifiers will modify the base parameters.
-- The first two modify the random generated size for barriers.
-- The last one modifies the random spawning distance between generators.
type WidthMod  = Int
type HeightMod = Int
type DistMod = [Int]
data DiffMod = DiffMod
  { _widthmod  :: WidthMod
  , _heightmod :: HeightMod
  , _distMod   :: DistMod
  } deriving (Eq, Show)

makeLenses ''DifficultyMap
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

-- Barrier min/max width/height
widthMin, widthMax, heightMin, heightMax :: Int
widthMin = 1
widthMax = 4
heightMin = 1
heightMax = 4

-- Update score every three frames
constScoreMod :: Int
constScoreMod = 2

-- Increase the difficulty everytime the score goes up by levelAmount.
levelAmount :: Score
levelAmount = 100

scoreMap :: M.Map Int Difficulty
scoreMap = M.fromList $ zip [0 ..] [D0 ..]

-- Functions
-- | Step forward in time.
-- Increment score every tick.
step :: Game -> Game
step g = fromMaybe g $ do
  guard $ not (g^.dead || g^.paused)
  return $ fromMaybe (step' g) (die g)

-- | What to do if we are not dead.
step' :: Game -> Game
step' = incDifficulty . setHighScore . incScore . move . spawnBarrier . deleteBarrier . adjustStanding

incScore :: Game -> Game
incScore g = case g^.scoreMod of
  0 -> g & score %~ (+1) & scoreMod %~ incAndMod
  _ -> g & scoreMod %~ incAndMod
  where incAndMod x = (x + 1) `mod` constScoreMod

setHighScore :: Game -> Game
setHighScore g = if g^.score > g^.highscore
                   then g & highscore .~ (g^.score)
                   else g

-- | Get game's relevant diff mod.
getDiffMod :: Game -> DiffMod
getDiffMod g = case g^.level of
  D0 -> g^.diffMap.d0
  D1 -> g^.diffMap.d1
  D2 -> g^.diffMap.d2
  D3 -> g^.diffMap.d3
  D4 -> g^.diffMap.d4
  D5 -> g^.diffMap.d5
  D6 -> g^.diffMap.d6

-- | Set game's relevant diff mod.
setDiffMod :: DiffMod -> Game -> Game
setDiffMod dm g = case g^.level of
  D0 -> g & diffMap.d0 .~ dm
  D1 -> g & diffMap.d1 .~ dm
  D2 -> g & diffMap.d2 .~ dm
  D3 -> g & diffMap.d3 .~ dm
  D4 -> g & diffMap.d4 .~ dm
  D5 -> g & diffMap.d5 .~ dm
  D6 -> g & diffMap.d6 .~ dm

-- | Convert score to difficulty level
scoreToDiff :: Score -> Difficulty
scoreToDiff sc = let l = sc `div` levelAmount
                    in fromMaybe D6 (M.lookup l scoreMap)
-- scoreToDiff sc = D4

-- | Increase the game's (difficulty) level. We'll increase it every
-- levelAmount points.
incDifficulty :: Game -> Game
incDifficulty g = g & level .~ scoreToDiff (g^.score)

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
--
--
spawnBarrier :: Game -> Game
spawnBarrier g =
  -- Width, height, distance modifiers
  let (DiffMod wm hm (d:ds)) = getDiffMod g
      newDiffMod = DiffMod wm hm ds
  in case viewr $ g^.barriers of
    EmptyR -> addRandomBarrier g
    _ :> a -> let x = getBarrierLeftmost a in
                if (gridWidth - x) > d then setDiffMod newDiffMod (addRandomBarrier g) else g

getBarrierLeftmost :: Barrier -> Int
getBarrierLeftmost [] = 0
getBarrierLeftmost (V2 x _:_) = x

getBarrierRightmost :: Barrier -> Int
getBarrierRightmost [] = gridWidth
getBarrierRightmost b = let (V2 x _) = last b in x

-- | Add random barrier (constrained random width/height).
-- Choose between ground barrier and sky barrier.
addRandomBarrier :: Game -> Game
addRandomBarrier g =
  let (p:ps) = g^.positions
  in case p of
    Sky    -> addRandomGroundBarrier g & positions .~ ps
    Ground -> addRandomGroundBarrier g & positions .~ ps

-- | Add random ground barrier (ypos is 0)
-- Height and width of random barrier are the minimum of the current random
-- coordinate and the current diffmod's height and width.
-- E.g. at D0, everything will be w=1, h=1
-- and at D6, heights and widths will range from 1 to 3
addRandomGroundBarrier :: Game -> Game
addRandomGroundBarrier g =
  let (V2 w h:rest) = g^.dimns
      (DiffMod wm hm _) = getDiffMod g -- UNSAFE!
      newBarrier = makeBarrier (V2 (min w wm) (min h hm)) 0
  in g & barriers %~ (|> newBarrier) & dimns .~ rest

-- | Add random sky barrier (ypos is 1)
addRandomSkyBarrier :: Game -> Game
addRandomSkyBarrier g =
  let (V2 w h:rest) = g^.dimns
      (DiffMod wm hm _) = getDiffMod g -- UNSAFE!
      newBarrier = makeBarrier (V2 (min w wm) (min h hm)) 1
  in g & barriers %~ (|> newBarrier) & dimns .~ rest

-- | Make a barrier. The width and height are determined by
-- the dimension, and the second parameter determines the
-- starting ypos.
makeBarrier :: Dimension -> Int -> Barrier
makeBarrier (V2 w h) y =
  [V2 (gridWidth + a) (y + b) | a <- [0..w-1], b <- [0..h-1]]

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
initGame :: Score -> IO Game
initGame hs = do
  dimensions      <- randomRs (V2 widthMin heightMin, V2 widthMax heightMax) <$> newStdGen
  randomPositions <- flip weightedList ((Sky, 1 % 4) : replicate 3 (Ground, 1 % 4)) <$> newStdGen
  dMap            <- difficultyMap
  let g = Game { _dino      = standingDino
               , _dir       = Still
               , _barriers  = S.empty
               , _dimns     = dimensions
               , _positions = randomPositions
               , _level     = D0
               , _diffMap   = dMap
               , _paused    = False
               , _dead      = False
               , _scoreMod  = 0
               , _score     = 0
               , _highscore = hs
               }
  return g

difficultyMap :: IO DifficultyMap
difficultyMap = do
  dists0 <- randomRs (20, 25) <$> newStdGen
  dists1 <- randomRs (17, 22) <$> newStdGen
  dists2 <- randomRs (16, 20) <$> newStdGen
  dists3 <- randomRs (15, 18) <$> newStdGen
  dists4 <- randomRs (12, 16) <$> newStdGen
  return $ DifficultyMap
    (DiffMod 1 1 dists0)
    (DiffMod 1 2 dists1)
    (DiffMod 2 2 dists2)
    (DiffMod 2 3 dists3)
    (DiffMod 3 3 dists4) -- same dists, different widths/heights
    (DiffMod 3 4 dists4)
    (DiffMod 4 4 dists4)

weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weights = evalRand m gen
    where m = sequence . repeat . fromList $ weights

instance Random a => Random (V2 a) where
  randomR (V2 x1 y1, V2 x2 y2) g =
    let (x, g')  = randomR (x1, x2) g
        (y, g'') = randomR (y1, y2) g'
     in (V2 x y, g'')
  random g =
    let (x, g')  = random g
        (y, g'') = random g'
     in (V2 x y, g'')
