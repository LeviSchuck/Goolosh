{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.SDL.Init where

import System.IO
import Data.String(IsString(..),String)
import Control.Monad(Monad(..))

import qualified SDL

import Goolosh.SDL.State

initSDL :: String -> IO SDLState
initSDL title = do
    SDL.initialize
        [ SDL.InitTimer
        , SDL.InitVideo
        , SDL.InitEvents
        ]
    -- 
    let windowTitle = fromString title
    let windowConfig = SDL.defaultWindow
    let renderConfig = SDL.defaultRenderer
    --
    window <- SDL.createWindow windowTitle windowConfig
    renderer <- SDL.createRenderer window (-1) renderConfig
    let ret = SDLState
            { sdlWindow = window
            , sdlRenderer = renderer
            }
    return ret

--