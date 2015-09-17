{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.SDL.State where

import qualified SDL

data SDLState = SDLState
    { sdlWindow :: SDL.Window
    , sdlRenderer :: SDL.Renderer
    }

--
