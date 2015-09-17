{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.Game.Entity where

import Prelude(Eq(..),Show(..),Int,Ord(..))

data MobType
    = MobTubeThing
    | MobWalker
    deriving (Show,Eq)

-- TODO
data MaterialKind = MaterialKind
    deriving (Show,Eq)
data TileKind = TileKind
    deriving (Show,Eq)

data SolidDetails
    = SolidParent
    | SolidPipe
    | SolidMaterial MaterialKind
    | SolidTile TileKind
    deriving (Show,Eq)

data EntityKind
    = EntityScene
    | EntityLayer Int
    | EntityPlayer
    | EntityMob MobType
    | EntitySolid SolidDetails
    deriving (Show,Eq)

data EntityChildKey
    = ECKPlayer
    | ECKScene
    | ECKTile Int
    | ECKPlatform Int
    deriving(Show,Eq,Ord)

data AnimationKind
    = AnimNone
    | AnimHorizMove
    | AnimHorizRun
    | AnimVertUp
    | AnimVertDown
    | AnimIdle
    | AnimCrouch
    deriving (Show,Eq,Ord)

data EntityAnimation
    = EntityAnimation
        { animationFrame :: Int
        , animationKind :: AnimationKind
        }
    | EntityStaticAnimation
    deriving (Show,Eq)

data Entity = Entity
    { entityKind :: EntityKind
    , entityAnimation :: EntityAnimation
    } deriving (Show,Eq)
