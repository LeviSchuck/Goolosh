{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Goolosh.SDL.Output.BasicRaster where

import Prelude(($),floor,Num(..),Eq(..),fromIntegral,Ord(..))
import Control.Monad(Monad(..),forM_,when,unless)
import Data.Functor
import Data.Maybe

import qualified SDL
import SDL(($=))
import Linear ((*!))
import qualified Linear as L
import qualified Linear.Affine as LA
import Control.Monad.IO.Class

import Goolosh.Game.Output
import Goolosh.SDL.State
import Goolosh.SDL.Output.Color
import Goolosh.Geom.Drawable
import Goolosh.Geom.Transform
import Goolosh.Geom.SceneGraph

gameNaiveRaster :: MonadIO m => SDLState -> GameOutput -> m ()
gameNaiveRaster SDLState{..} g = forM_ s $ \n -> when (isJust (_nodeDrawQuad n)) $ do
    let L.V2 r1 r2 = _nodeBoundingBox n
        (L.V2 x1 y1) = fmap floor r1
        (L.V2 x2 y2) = fmap floor r2
        coords = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
        ne = _nodeEntity n
        nit = _nodeInverseTransform n
    forM_ coords $ \(x,y) -> do
        let p = convertToPoint $ make2DPoint (fi x) (fi y)
            c = drawSample ne $ (p *! nit)
            c2 = if colorAlpha c < 1
                then mixColor c backGroundColor
                else c
            sdlColor = toRGBA32 c2
            sdlPoint = LA.P $ L.V2 x y
        unless (colorAlpha c == 0) $ do
            SDL.rendererDrawColor sdlRenderer $= sdlColor
            SDL.drawPoint sdlRenderer sdlPoint
    where
        s = _sceneNodes $ collapseGraph $ _gameScene g
        fi = fromIntegral





--