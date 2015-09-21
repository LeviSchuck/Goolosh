{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Goolosh.Game.Interpret where

import Prelude(($),Num(..),Floating(..),Float,(.))
import Control.Monad
import Data.Maybe

import qualified Data.Conduit as C
import qualified Data.Sequence as S
import Data.Sequence((|>))
import Control.Monad.Trans.State.Strict as ST
import Control.Lens((&),(.~))

import Goolosh.Game.State
import Goolosh.Geom.Transform
import Goolosh.Geom.SceneGraph
import Goolosh.Game.Entity
import Goolosh.Game.Output

stateToScene :: Monad m => C.Conduit GameState m GameOutput
stateToScene = loop
    where
        loop = do
            s <- C.await
            case s of
                Nothing -> return ()
                Just s' -> do
                    C.yield $ genOutput s'
                    loop
        genOutput s = baseOutput & (gameScene . sceneChildren) .~ sceneFromState s
        mobs :: GameState -> SceneGraph Entity
        mobs s = SceneGraph
            { _sceneGraphNode = Entity
                { _entityKind = EntityLayer 1
                , _entityAnimation = EntityStaticAnimation
                }
            , _sceneTransform = affineIdentity
            , _sceneDrawBox = Nothing
            , _sceneChildren = scMap (gameMobs s) $ \m -> SceneGraph
                { _sceneGraphNode = Entity
                    { _entityKind = EntityMob MobWalker
                    , _entityAnimation = EntityStaticAnimation
                    }
                , _sceneChildren = S.empty
                , _sceneTransform
                    = affineTranslate (make2DPoint 1 1)
                    $ affineScale (make2DPoint 0.5 0.5)
                    $ affineTranslate (mobPosition m)
                    $ affineScale sf
                    $ affineIdentity
                , _sceneDrawBox = Just identityBB
                }
            }
        tiles :: GameState -> SceneGraph Entity
        tiles s = SceneGraph
            { _sceneGraphNode = Entity
                { _entityKind = EntityLayer 2
                , _entityAnimation = EntityStaticAnimation
                }
            , _sceneTransform = affineIdentity
            , _sceneDrawBox = Nothing
            , _sceneChildren = scMap (gameTiles s) $ \t -> SceneGraph
                { _sceneGraphNode = Entity
                    { _entityKind = EntitySolid $ SolidTile TileKind
                    , _entityAnimation = EntityStaticAnimation
                    }
                , _sceneChildren = S.empty
                , _sceneTransform
                    = affineTranslate (make2DPoint 1 1)
                    $ affineScale (make2DPoint 0.5 0.5)
                    $ affineTranslate (tilePosition t)
                    $ affineScale sf
                    $ affineIdentity
                , _sceneDrawBox = Just identityBB
                }
            }
        player :: GameState -> SceneGraph Entity
        player s = SceneGraph
            { _sceneGraphNode = Entity
                { _entityKind = EntityLayer 3
                , _entityAnimation = EntityStaticAnimation
                }
            , _sceneTransform = affineIdentity
            , _sceneDrawBox = Nothing
            , _sceneChildren = S.empty |> SceneGraph 
                { _sceneGraphNode = Entity
                    { _entityKind = EntityPlayer
                    , _entityAnimation = EntityStaticAnimation
                    }
                , _sceneChildren = S.empty
                , _sceneTransform
                    = affineTranslate (make2DPoint 1 1)
                    $ affineScale (make2DPoint 0.5 0.5)
                    $ affineTranslate (playerPosition $ gamePlayer s)
                    $ affineScale sf
                    $ affineIdentity
                , _sceneDrawBox = Just identityBB
                }
            }
        sceneFromState :: GameState -> S.Seq (SceneGraph Entity)
        sceneFromState s = S.empty
            |> mobs s
            |> tiles s
            |> player s
        scMap d f = fmap f d
        sf = make2DPoint 20 20

baseOutput :: GameOutput
baseOutput = GameOutput
    { _gameScene = SceneGraph 
        { _sceneGraphNode = Entity
            { _entityKind = EntityScene
            , _entityAnimation = EntityStaticAnimation
            }
        , _sceneChildren = S.empty
        , _sceneTransform = affineIdentity
        , _sceneDrawBox = Nothing
        }
    , _gameView = makeBB (make2DPoint 0 0) (make2DPoint 100 100)
    }

--