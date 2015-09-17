{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Goolosh.Geom.Output.Quad where


import Prelude(Show(..),Eq(..),($),Num(..))
import qualified Linear as L
import Data.Foldable
import Data.Functor
import Control.Monad(Monad(..),unless)
import Data.Maybe

import qualified Data.Sequence  as S
import Data.Sequence ((|>))
import Control.Monad.Trans.State.Strict as ST

import Goolosh.Geom.Transform
import Goolosh.Geom.SceneGraph



data QuadKind
    = DrawQuad
    | BoundingQuad
    deriving(Eq, Show)


graphToQuads :: SceneGraph a -> S.Seq (GMBQ, QuadKind)
graphToQuads g = ST.execState s S.empty
    where
        cg = sceneNodes $ collapseGraph g
        s  = do
            mapM_ conv cg
        conv SceneNode{..} = do
            let bb = boundingBoxToQuad nodeBoundingBox
            case nodeDrawQuad of
                Nothing -> ST.modify (|> (bb, BoundingQuad))
                Just q -> do
                    let bbq = bb - q
                        nz = L.nearZero bbq
                    ST.modify (|> (q, DrawQuad))
                    unless nz $ ST.modify (|> (bb, BoundingQuad))

--