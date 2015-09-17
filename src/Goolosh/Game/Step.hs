{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.Game.Step where

import Prelude(($))
import Control.Monad
import Control.Concurrent
import Data.Maybe

import qualified Data.Conduit as C
import Control.Monad.IO.Class

import Goolosh.Game.Input
import Goolosh.Game.Output
import Goolosh.Game.State
import Goolosh.Geom.Transform




gameStep :: MonadIO m => GameState -> C.Conduit GameInput m GameState
gameStep s = do
    i' <- C.await
    case i' of 
        Nothing -> return ()
        Just Terminate -> return ()
        Just (TimeUnit t) -> do
            liftIO $ threadDelay t
            C.yield s
            gameStep s
        Just _ -> do
            C.yield s
            gameStep s


--