{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Goolosh.Geom.SceneGraph where

import Prelude(Show(..),Eq(..),($))
import Data.Foldable
import Data.Functor
import Control.Monad(Monad(..),mapM,unless)
import Data.Maybe

import qualified Data.Sequence  as S
import Data.Sequence ((|>))
import Control.Monad.Trans.State.Strict as ST
import Control.Lens.TH(makeLenses)

import Goolosh.Geom.Transform

data SceneGraph a = SceneGraph
    { _sceneGraphNode :: a
    , _sceneChildren :: S.Seq (SceneGraph a)
    , _sceneTransform :: GM32
    , _sceneDrawBox :: Maybe GMBB
    } deriving (Show, Eq)

makeLenses ''SceneGraph

data SceneNode a = SceneNode
    { _nodeEntity :: a
    , _nodeTransform :: GM32
    , _nodeDrawQuad :: Maybe GMBQ
    , _nodeBoundingBox :: GMBB
    , _nodeInverseTransform :: GM32
    } deriving (Show, Eq)

makeLenses ''SceneNode

data SceneTest a
    = SceneNotFound
    | SceneFound GV2D (SceneGraph a)
    deriving (Show, Eq)

data Scene a = Scene
    { _sceneNodes :: S.Seq (SceneNode a)
    } deriving (Show, Eq)
makeLenses ''Scene

collapseGraph :: SceneGraph a -> Scene a
collapseGraph g = Scene
    { _sceneNodes = ns }
    where
        ns = ST.execState s S.empty
        s = do
            _ <- conv affineIdentity g
            return ()
        conv :: GM32 -> SceneGraph a -> ST.State (S.Seq (SceneNode a)) (Maybe GMBB)
        conv t SceneGraph{..} = do
            let t'  = _sceneTransform @!*! t
                sdb = fmap (transformBBtoQuad t') _sceneDrawBox
                sbb = fmap quadToBoundingBox sdb
            bbs0 <- mapM (conv t') _sceneChildren
            let bbs1 = S.filter isJust bbs0
                bbs2 = fmap fromJust bbs1
                bbs = case sbb of
                    Nothing -> bbs2
                    Just sb -> bbs2 |> sb
                bb' = if S.null bbs
                    then Nothing
                    else Just $ foldl1 mergeBB bbs
            case bb' of
                Nothing -> return Nothing
                Just bb -> do
                    ST.modify $ \n -> n |> SceneNode
                        { _nodeEntity = _sceneGraphNode
                        , _nodeTransform = t'
                        , _nodeDrawQuad = sdb
                        , _nodeBoundingBox = bb
                        , _nodeInverseTransform = affineInverse t'
                        }
                    return $ Just bb


--