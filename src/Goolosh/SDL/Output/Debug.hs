{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Goolosh.SDL.Output.Debug where

import Prelude(($),floor)
import Control.Monad(Monad(..),forM_)
import Data.Functor


import qualified SDL
import SDL(($=))
import qualified Linear as L
import qualified Linear.Affine as LA
import Control.Monad.IO.Class

import Goolosh.Game.Output
import Goolosh.Geom.Output.Quad
import Goolosh.SDL.State


gameDrawDebug :: MonadIO m => SDLState -> GameOutput -> m ()
gameDrawDebug SDLState{..} game = do
    forM_ (graphToQuads (_gameScene game)) $ \(q, k) -> do
        let L.V4 p1 p2 p3 p4 = fmap ff q
        let ls = [(p1,p2),(p2,p3),(p3,p4),(p4,p1)]
        ls2 <- case k of
            DrawQuad -> do
                sc white
                return ((p2,p4):(p1,p3):ls)
            BoundingQuad -> do
                sc red
                return ls
        forM_ ls2 $ \(pa,pb) -> do
            SDL.drawLine sdlRenderer (LA.P pa) (LA.P pb)
    where
        ff = fmap floor
        sc c = SDL.rendererDrawColor sdlRenderer $= c
        white = L.V4 255 255 255 150
        red = L.V4 255 0 0 100

--