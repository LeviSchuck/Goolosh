{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Goolosh.Game.Output where

import Prelude(Show(..),Eq(..))

import Control.Lens.TH(makeLenses)

import Goolosh.Geom.Transform
import Goolosh.Game.Entity
import Goolosh.Geom.SceneGraph


data GameOutput = GameOutput
    { _gameScene :: SceneGraph Entity
    , _gameView :: GMBB
    } deriving (Show,Eq)

makeLenses ''GameOutput

--