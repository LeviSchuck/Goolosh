{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.Game.Input where

import Prelude(Show(..),Eq(..),Ord(..),Int)

data PlayerInput
    = Jump
    | Shoot
    | GoLeft
    | GoRight
    | Down
    deriving (Show,Eq,Ord)

data InputKind
    = KeyPressed
    | KeyReleased
    deriving (Show,Eq,Ord)

data GameInput
    = Terminate
    | TimeUnit Int
    | PlayerCommand InputKind PlayerInput
    deriving (Show,Eq,Ord)


--