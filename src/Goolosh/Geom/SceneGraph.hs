{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Goolosh.Geom.SceneGraph where

import Prelude(Show(..),Eq(..),($),Num(..))
import qualified Linear as L
import Data.Foldable
import Data.Functor
import Control.Monad(Monad(..),mapM,unless)
import Data.Maybe

import qualified Data.Sequence  as S
import Data.Sequence ((|>))
import Control.Monad.Trans.State.Strict as ST

import Goolosh.Geom.Transform

data SceneGraph a = SceneGraph
    { sceneGraphNode :: a
    , sceneChildren :: S.Seq (SceneGraph a)
    , sceneTransform :: GM32
    , sceneDrawBox :: Maybe GMBB
    } deriving (Show, Eq)

data SceneNode a = SceneNode
    { nodeEntity :: a
    , nodeTransform :: GM32
    , nodeDrawQuad :: Maybe GMBQ
    , nodeBoundingBox :: GMBB
    } deriving (Show, Eq)

data SceneTest a
    = SceneNotFound
    | SceneFound GV2D (SceneGraph a)
    deriving (Show, Eq)

data Scene a = Scene
    { sceneNodes :: S.Seq (SceneNode a)
    } deriving (Show, Eq)


collapseGraph :: SceneGraph a -> Scene a
collapseGraph g = Scene
    { sceneNodes = ns }
    where
        ns = ST.execState s S.empty
        s = do
            _ <- conv affineIdentity g
            return ()
        conv :: GM32 -> SceneGraph a -> ST.State (S.Seq (SceneNode a)) (Maybe GMBB)
        conv t SceneGraph{..} = do
            let t'  = sceneTransform @!*! t
                sdb = fmap (transformBBtoQuad t') sceneDrawBox
                sbb = fmap quadToBoundingBox sdb
            bbs0 <- mapM (conv t') sceneChildren
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
                        { nodeEntity = sceneGraphNode
                        , nodeTransform = t'
                        , nodeDrawQuad = sdb
                        , nodeBoundingBox = bb
                        }
                    return $ Just bb


--