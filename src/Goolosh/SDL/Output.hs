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
import Goolosh.SDL.Output.Debug

gameOutputSink :: SDLState -> C.Sink GameOutput IO ()
gameOutputSink s@SDLState{..} = do
    game' <- C.await
    SDL.rendererDrawColor sdlRenderer $= L.V4 0 0 255 255
    SDL.clear sdlRenderer
    case game' of
        Nothing -> return ()
        Just game -> do
            gameDrawDebug s game
            SDL.present sdlRenderer
            gameOutputSink s

--