{-# LANGUAGE RankNTypes #-}

module Controls where

import Dino

import Lens.Micro ((&), (.~), (%~), (^.))

handleUp :: Game -> Game
handleUp = changeDir Up

handleDown :: Game -> Game
handleDown = changeDir Down

changeDir :: Direction -> Game -> Game
changeDir d g = g & dir .~ d
