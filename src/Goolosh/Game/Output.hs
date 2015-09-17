{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.Game.Output where

import Prelude(Show(..),Eq(..))

import Goolosh.Geom.Transform
import Goolosh.Game.Entity
import Goolosh.Geom.SceneGraph


data GameOutput = GameOutput
    { gameScene :: SceneGraph Entity
    , gameView :: GMBB
    } deriving (Show,Eq)

--