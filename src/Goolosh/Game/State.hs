{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.Game.State where

import Prelude(Show(..),Eq(..),Ord(..),Int)

import qualified Data.Map as M
import qualified Data.Sequence as S

import Goolosh.Geom.Transform

data PlayerState
    = Halted
    | PlayerDeath
    | Invincible
    | Normal
    deriving (Show,Eq,Ord)

data SceneState
    = LevelStart
    | LevelEnd
    | GameDeath
    | Transition
    deriving (Show,Eq,Ord)

data MobState
    = MobMoving
    | MobDeath
    | Hiding
    | MobStill
    | Revealing
    deriving (Show,Eq,Ord)

data TileState
    = Immutable
    | Ivisible
    | Visible
    | Breaking
    | Destroyed
    deriving (Show,Eq,Ord)

data ItemState
    = Spawnable
    | ItemStill
    | ItemMoving
    | Consumed
    deriving (Show,Eq,Ord)

data MusicState
    = OneTrack
    | Silence
    | LevelLoop
    | TemporaryTrack
    deriving (Show,Eq,Ord)

data MobExtra
    = ExtraFire
    | ExtraWater

data GamePlayer = GamePlayer
    { playerPosition :: GV2D
    , playerState :: PlayerState
    }

data GameMob = GameMob
    { mobPosition :: GV2D
    , mobState :: MobState
    , mobExtras :: S.Seq MobExtra
    }

data GameState = GameState
    { gamePlayer :: GamePlayer
    , gameMobs :: M.Map Int GameMob
    }

dummyState = GameState
    { gamePlayer = GamePlayer
        { playerPosition = make2DPoint 0 0
        , playerState = Normal
        }
    , gameMobs = M.empty
    }
--
