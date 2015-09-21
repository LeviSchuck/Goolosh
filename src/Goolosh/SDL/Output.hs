{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Goolosh.SDL.Output where

import Control.Monad(Monad(..))
import System.IO
import Data.Maybe


import qualified Data.Conduit as C
import qualified SDL
import SDL(($=))
import qualified Linear as L

import Goolosh.Game.Output
import Goolosh.SDL.State
import Goolosh.SDL.Output.Color
import Goolosh.SDL.Output.Debug
import Goolosh.SDL.Output.BasicRaster
import Goolosh.Geom.Drawable

gameOutputSink :: SDLState -> C.Sink GameOutput IO ()
gameOutputSink s@SDLState{..} = do
    game' <- C.await
    SDL.rendererDrawColor sdlRenderer $= sdlBG
    SDL.clear sdlRenderer
    case game' of
        Nothing -> return ()
        Just game -> do
            gameNaiveRaster s game
            -- gameDrawDebug s game
            SDL.present sdlRenderer
            gameOutputSink s
    where
        bg = backGroundColor
        sdlBG = toRGBA32 bg

--