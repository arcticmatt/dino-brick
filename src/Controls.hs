{-# LANGUAGE RankNTypes #-}

module Controls where

import Dino (Game(..), Direction(..), dir)

import Lens.Micro ((&), (.~), (^.))

handleUp :: Game -> Game
handleUp g = if g^.dir == Still || g^.dir == Duck then changeDir Up g else g

handleDown :: Game -> Game
handleDown g = if g^.dir == Still || g^.dir == Down then changeDir Duck g else g

changeDir :: Direction -> Game -> Game
changeDir d g = g & dir .~ d
