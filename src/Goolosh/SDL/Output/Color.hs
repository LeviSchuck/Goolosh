{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Goolosh.SDL.Output.Color where


import Prelude(Num(..),Integral(..),RealFrac(..),($))

import qualified Linear as L

import Goolosh.Geom.Drawable

toRGB8 :: Integral a => ColorType -> a
{-# INLINE toRGB8 #-}
toRGB8 c = floor $ clamp 0 255 $ c * 255

toRGBA32 :: Integral a => DrawableColor -> L.V4 a
{-# INLINE toRGBA32 #-}
toRGBA32 DrawableColor{..} = L.V4
    (toRGB8 colorRed)
    (toRGB8 colorGreen)
    (toRGB8 colorBlue)
    (toRGB8 colorAlpha)

--