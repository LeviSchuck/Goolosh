{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Goolosh.Game.Entity where

import Prelude(Eq(..),Show(..),Int,Ord(..))

import Control.Lens.TH(makeLenses)

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
        { _animationFrame :: Int
        , _animationKind :: AnimationKind
        }
    | EntityStaticAnimation
    deriving (Show,Eq)
makeLenses ''EntityAnimation

data Entity = Entity
    { _entityKind :: EntityKind
    , _entityAnimation :: EntityAnimation
    } deriving (Show,Eq)
makeLenses ''Entity
