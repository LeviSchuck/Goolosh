{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.Geom.Drawable where

import Prelude(Ord(..),Num(..),Float(..))
import Data.Monoid

import Goolosh.Geom.Transform
import Goolosh.Game.Entity

type ColorType = Float

data DrawableColor = DrawableColor 
    { colorRed      :: {-# UNPACK #-} !ColorType
    , colorGreen    :: {-# UNPACK #-} !ColorType
    , colorBlue     :: {-# UNPACK #-} !ColorType
    , colorAlpha    :: {-# UNPACK #-} !ColorType
    }

backGroundColor :: DrawableColor
backGroundColor = DrawableColor 0 0 0 1

emptyColor :: DrawableColor
emptyColor = DrawableColor 0 0 0 0

clamp :: Ord a => a -> a -> a -> a
clamp mi ma v = min ma (max mi v)

-- ! Mix from left on top of right.
mixColor :: DrawableColor -> DrawableColor -> DrawableColor
mixColor c1@DrawableColor { colorAlpha = 1 } _ = c1
mixColor (DrawableColor ar ag ab aa) (DrawableColor br bg bb ba)
    = DrawableColor mr mg mb ma
    where
        c = clamp 0 1
        aa' = 1 - aa
        mr = c (br * ba * aa' + ar * aa)
        mg = c (bg * ba * aa' + ag * aa)
        mb = c (bb * ba * aa' + ab * aa)
        ma = c (     ba * aa' +      aa)

instance Monoid DrawableColor where
    mempty = emptyColor
    mappend = mixColor

class Drawable a where
    drawSample :: a -> GV2D -> DrawableColor


instance Drawable Entity where
    drawSample Entity { entityKind = EntityScene } _ = DrawableColor 0.1 0.1 0.1 1
    drawSample Entity { entityKind = EntityLayer _ } _ = DrawableColor 0.14 0.15 0.15 0.1
    drawSample Entity { entityKind = EntityPlayer } _ = DrawableColor 1 0 0 1
    drawSample _ _ = DrawableColor 0.5 0.5 0.5 0.3 

--
