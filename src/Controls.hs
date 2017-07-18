{-# LANGUAGE RankNTypes #-}

module Controls where

import Stairs
import Lens.Micro.Type
import Lens.Micro ((&), (.~), (^.))

-- | Change direction of a foot, which is referenced by dl.
-- If current direction is Still, we change it to Up.
-- We can't move a foot if the other foot is not Still.
changeFoot :: Lens' Game Direction -> Lens' Game Direction -> Game -> Game
changeFoot _ other g | g^.other /= Still = g
changeFoot tomove _ g = case g^.tomove of
  -- Up    -> g & tomove .~ Down -- for now, you can't manually go down
  Still -> g & tomove .~ Up
  _     -> g -- do nothing if foot is moving down or up

changeRight :: Game -> Game
changeRight = changeFoot rDir lDir

changeLeft :: Game -> Game
changeLeft = changeFoot lDir rDir
