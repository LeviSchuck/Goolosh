{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.SDL.Input where

import Prelude(Maybe(..),($),(.),Num(..),Show(..),Int)

import qualified Data.Conduit as C
import qualified SDL
import Control.Applicative((<$>))
import Control.Monad.IO.Class
import Control.Monad
import System.IO

import Goolosh.Game.Input

gameInputSource :: C.Source IO GameInput
gameInputSource = do
    e <- liftIO $ SDL.pollEvent
    let ev = case e of
            Nothing -> Nothing
            Just ex -> Just $ SDL.eventPayload ex 
    C.yield $ case lookEvent ev of
        Nothing -> TimeUnit 500
        Just x -> x
    gameInputSource
    where
        pressed e = case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
            SDL.ScancodeUp -> Just Jump
            SDL.ScancodeDown -> Just Down
            SDL.ScancodeRight -> Just GoRight
            SDL.ScancodeLeft -> Just GoLeft
            SDL.ScancodeSpace -> Just Jump
            _ -> Nothing
        lookEvent (Just SDL.QuitEvent) = Just Terminate
        lookEvent (Just (SDL.KeyboardEvent e)) = case SDL.keyboardEventKeyMotion e of
            SDL.Pressed -> PlayerCommand KeyPressed <$> pressed e
            SDL.Released -> PlayerCommand KeyReleased <$> pressed e
        lookEvent _ = Nothing

inputThing :: Int -> C.Sink GameInput IO ()
inputThing 0 = return ()
inputThing l = do
    e <- C.await
    liftIO . putStrLn . show $ e
    inputThing (l-1)
