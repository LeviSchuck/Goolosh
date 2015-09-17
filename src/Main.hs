{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.IO

import Data.Conduit(($$),(=$=),($=),(=$))

import Goolosh.SDL.Init
import Goolosh.SDL.Output
import Goolosh.SDL.Input
import Goolosh.Game.Output
import Goolosh.Game.Interpret
import Goolosh.Game.Step
import Goolosh.Game.State



main :: IO ()
main = do
    putStrLn "Hai"
    sdl <- initSDL "Heyyo"
    let out = gameOutputSink sdl
        step = gameStep dummyState
        input = gameInputSource

    input $$ step =$= stateToScene $= out


