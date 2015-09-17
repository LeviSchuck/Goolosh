{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.Game.Interpret where

import Prelude(($),Num(..),Floating(..))
import Control.Monad
import Data.Maybe

import qualified Data.Conduit as C
import qualified Data.Sequence as S
import Data.Sequence((|>))

import Goolosh.Game.State
import Goolosh.Game.Output
import Goolosh.Geom.Transform
import Goolosh.Geom.SceneGraph
import Goolosh.Game.Entity


dummyOutput amt = GameOutput
    { gameScene = SceneGraph 
        { sceneGraphNode = Entity
            { entityKind = EntityScene
            , entityAnimation = EntityStaticAnimation
            }
        , sceneChildren = S.empty
            |> SceneGraph 
                { sceneGraphNode = Entity
                    { entityKind = EntityPlayer
                    , entityAnimation = EntityStaticAnimation
                    }
                , sceneChildren = S.empty
                    |> SceneGraph 
                        { sceneGraphNode = Entity
                            { entityKind = EntityPlayer
                            , entityAnimation = EntityStaticAnimation
                            }
                        , sceneChildren = S.empty
                        , sceneTransform
                            = affineScale (make2DPoint 0.2 0.2)
                            $ affineTranslate (make2DPoint 1 1)
                            $ affineIdentity
                        , sceneDrawBox = Just identityBB
                        }
                , sceneTransform
                    = affineScale (make2DPoint 30 40)
                    $ affineRotateRad (amt * 1.185 + 0.4)
                    $ affineTranslate (make2DPoint (135 + sin (amt * 0.3) * 100) 60)
                    $ affineIdentity
                , sceneDrawBox = Just identityBB
                }
            |> SceneGraph
                { sceneGraphNode = Entity
                    { entityKind = EntityLayer 1
                    , entityAnimation = EntityStaticAnimation
                    }
                , sceneChildren = S.empty
                    |> SceneGraph
                        { sceneGraphNode = Entity
                            { entityKind = EntityMob MobWalker
                            , entityAnimation = EntityStaticAnimation
                            }
                        , sceneChildren = S.empty
                        , sceneTransform
                            = affineScale (make2DPoint 5 5)
                            $ affineTranslate (make2DPoint 5 0)
                            $ affineIdentity
                        , sceneDrawBox = Just identityBB
                        }
                    |> SceneGraph
                        { sceneGraphNode = Entity
                            { entityKind = EntityMob MobWalker
                            , entityAnimation = EntityStaticAnimation
                            }
                        , sceneChildren = S.empty
                        , sceneTransform
                            = affineRotateRadOrigin amt
                            $ affineScale (make2DPoint 5 5)
                            $ affineTranslate (make2DPoint 15 0)
                            $ affineIdentity
                        , sceneDrawBox = Just identityBB
                        }
                    |> SceneGraph
                        { sceneGraphNode = Entity
                            { entityKind = EntityMob MobWalker
                            , entityAnimation = EntityStaticAnimation
                            }
                        , sceneChildren = S.empty
                        , sceneTransform
                            = affineScale (make2DPoint 5 5)
                            $ affineTranslate (make2DPoint 25 0)
                            $ affineIdentity
                        , sceneDrawBox = Just identityBB
                        }
                , sceneTransform
                    = affineScale (make2DPoint 3 3)
                    $ affineTranslate (make2DPoint 80 140)
                    $ affineIdentity
                , sceneDrawBox = Nothing
                }
        , sceneTransform = affineIdentity
        , sceneDrawBox = Nothing
        }
    , gameView = makeBB (make2DPoint 0 0) (make2DPoint 100 100)
    }

stateToScene :: Monad m => C.Conduit GameState m GameOutput
stateToScene = loop 0
    where
        loop amt = do
            let namt = amt+0.05
            s <- C.await
            case s of
                Nothing -> return ()
                Just _ -> do
                    C.yield $ dummyOutput namt
                    loop namt

--