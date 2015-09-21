module Goolosh.Game.Dummy.Interpret where


dummyOutput :: Float -> GameOutput
dummyOutput amt = GameOutput
    { _gameScene = SceneGraph 
        { _sceneGraphNode = Entity
            { _entityKind = EntityScene
            , _entityAnimation = EntityStaticAnimation
            }
        , _sceneChildren = S.empty
            |> SceneGraph 
                { _sceneGraphNode = Entity
                    { _entityKind = EntityPlayer
                    , _entityAnimation = EntityStaticAnimation
                    }
                , _sceneChildren = S.empty
                    |> SceneGraph 
                        { _sceneGraphNode = Entity
                            { _entityKind = EntityPlayer
                            , _entityAnimation = EntityStaticAnimation
                            }
                        , _sceneChildren = S.empty
                        , _sceneTransform
                            = affineScale (make2DPoint 0.2 0.2)
                            $ affineTranslate (make2DPoint 1 1)
                            $ affineIdentity
                        , _sceneDrawBox = Just identityBB
                        }
                , _sceneTransform
                    = affineScale (make2DPoint 30 40)
                    $ affineRotateRad (amt * 1.185 + 0.4)
                    $ affineTranslate (make2DPoint (135 + sin (amt * 0.3) * 100) 60)
                    $ affineIdentity
                , _sceneDrawBox = Just identityBB
                }
            |> SceneGraph
                { _sceneGraphNode = Entity
                    { _entityKind = EntityLayer 1
                    , _entityAnimation = EntityStaticAnimation
                    }
                , _sceneChildren = S.empty
                    |> SceneGraph
                        { _sceneGraphNode = Entity
                            { _entityKind = EntityMob MobWalker
                            , _entityAnimation = EntityStaticAnimation
                            }
                        , _sceneChildren = S.empty
                        , _sceneTransform
                            = affineScale (make2DPoint 5 5)
                            $ affineTranslate (make2DPoint 5 0)
                            $ affineIdentity
                        , _sceneDrawBox = Just identityBB
                        }
                    |> SceneGraph
                        { _sceneGraphNode = Entity
                            { _entityKind = EntityMob MobWalker
                            , _entityAnimation = EntityStaticAnimation
                            }
                        , _sceneChildren = S.empty
                        , _sceneTransform
                            = affineRotateRadOrigin amt
                            $ affineScale (make2DPoint 5 5)
                            $ affineTranslate (make2DPoint 15 0)
                            $ affineIdentity
                        , _sceneDrawBox = Just identityBB
                        }
                    |> SceneGraph
                        { _sceneGraphNode = Entity
                            { _entityKind = EntityMob MobWalker
                            , _entityAnimation = EntityStaticAnimation
                            }
                        , _sceneChildren = S.empty
                        , _sceneTransform
                            = affineScale (make2DPoint 5 5)
                            $ affineTranslate (make2DPoint 25 0)
                            $ affineIdentity
                        , _sceneDrawBox = Just identityBB
                        }
                , _sceneTransform
                    = affineScale (make2DPoint 3 3)
                    $ affineTranslate (make2DPoint 80 140)
                    $ affineIdentity
                , _sceneDrawBox = Nothing
                }
        , _sceneTransform = affineIdentity
        , _sceneDrawBox = Nothing
        }
    , _gameView = makeBB (make2DPoint 0 0) (make2DPoint 100 100)
    }