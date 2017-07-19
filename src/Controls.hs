{-# LANGUAGE RankNTypes #-}

module Controls where

import Dino

import Lens.Micro ((&), (.~), (%~), (^.))

handleUp :: Game -> Game
handleUp g = if g^.dir == Still then changeDir Up g else g

handleDown :: Game -> Game
handleDown g = if g^.dir == Still then changeDir Down g else g

changeDir :: Direction -> Game -> Game
changeDir d g = g & dir .~ d
