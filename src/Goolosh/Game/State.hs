{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.Game.State where

import Prelude(Show(..),Eq(..),Ord(..),Int)

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Sequence((|>))

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

data GameTile = GameTile
    { tilePosition :: GV2D
    , tileState :: TileState
    }

data GameState = GameState
    { gamePlayer :: GamePlayer
    , gameMobs :: S.Seq GameMob
    , gameTiles :: S.Seq GameTile
    }

dummyState :: GameState
dummyState = GameState
    { gamePlayer = GamePlayer
        { playerPosition = make2DPoint 1 1
        , playerState = Normal
        }
    , gameMobs = S.empty
        |> GameMob
            { mobPosition = make2DPoint 5 5
            , mobState = MobStill
            , mobExtras = S.empty
            }
    , gameTiles = S.empty
        |> GameTile
            { tilePosition = make2DPoint 0 6
            , tileState = Immutable
            }
        |> GameTile
            { tilePosition = make2DPoint 1 6
            , tileState = Immutable
            }
        |> GameTile
            { tilePosition = make2DPoint 2 6
            , tileState = Immutable
            }
        |> GameTile
            { tilePosition = make2DPoint 3 6
            , tileState = Immutable
            }
        |> GameTile
            { tilePosition = make2DPoint 4 6
            , tileState = Immutable
            }
        |> GameTile
            { tilePosition = make2DPoint 5 6
            , tileState = Immutable
            }
        |> GameTile
            { tilePosition = make2DPoint 6 6
            , tileState = Immutable
            }
    }
--
