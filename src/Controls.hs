{-# LANGUAGE RankNTypes #-}

module Controls where

import Dino (Game(..), Direction(..), dir, duckCountdown, duckFrames, paused)

import Lens.Micro ((&), (.~), (^.), (%~))

handleUp :: Game -> Game
handleUp g = if g^.dir == Still || g^.dir == Duck then changeDir Up g else g

handleDown :: Game -> Game
handleDown g = if g^.dir == Still || g^.dir == Down
                 then changeDir Duck g & duckCountdown .~ duckFrames
                 else g

changeDir :: Direction -> Game -> Game
changeDir d g = g & dir .~ d

pause :: Game -> Game
pause g = g & paused %~ not
