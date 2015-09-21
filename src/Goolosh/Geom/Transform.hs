-- {-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Goolosh.Geom.Transform where

-- The fixed-point package looks useful if I want to not use floats

import Prelude(Float,Num(..),($),(.),Floating(..),Fractional(..),Eq(..),Functor(..))
import Data.Foldable
import Data.Ord
import Data.Bool

import Linear.Matrix((!*!),(*!),M42,M33,M32,M22,identity)
import Linear(R1(..),R2(..),R3(..),V4(..),V3(..),V2(..))
import Linear.Vector(scaled,unit,Additive(..))
import Control.Lens.Operators((&),(.~),(%~),(^.))
-- import Linear.Affine(Affine(..))
-- import Data.Foldable(Foldable(..))

type TransformUnit = Float

type GM33 = M33 TransformUnit
type GM32 = M32 TransformUnit
type GV2D = V2 TransformUnit
type GV3D = V3 TransformUnit
type GMBB = M22 TransformUnit
type GMBQ = M42 TransformUnit

make2DPoint :: TransformUnit -> TransformUnit -> GV2D
{-# INLINE make2DPoint #-}
make2DPoint = V2

originPoint :: GV3D
{-# INLINE originPoint #-}
originPoint = V3 0 0 1

makeBB :: GV2D -> GV2D -> GMBB
{-# INLINE makeBB #-}
makeBB v1 v2 = V2 v1 v2

identityBB :: GMBB
{-# INLINE identityBB #-}
identityBB = makeBB (make2DPoint (-1) (-1)) (make2DPoint 1 1)

convertToPoint :: GV2D -> GV3D
{-# INLINE convertToPoint #-}
convertToPoint (V2 x y) = V3 x y 1

convertToNormal :: GV2D -> GV3D
{-# INLINE convertToNormal #-}
convertToNormal (V2 x y) = V3 x y 0


rectToAffine :: GMBB -> GM32
{-# INLINE rectToAffine #-}
rectToAffine = affineInverse . rectToAffineInverse

rectToAffineInverse :: GMBB -> GM32
rectToAffineInverse (V2 (V2 x1 y1) (V2 x2 y2)) = sa . ss $ affineIdentity
    where
        (w, h) = (x2 - x1, y2 - y1)
        (w', h') = (w/2, h/2)
        sa = affineTranslate $ V2 (-x1 -w') (-y1 -h')
        ss = affineScale $ V2
            (1/(if w'== 0 then 1 else w'))
            (1/(if h'== 0 then 1 else h'))


transformBBtoQuad :: GM32 -> GMBB -> GMBQ
transformBBtoQuad t bb = transformQuad t $ boundingBoxToQuad bb

transformQuad :: GM32 -> GMBQ -> GMBQ
transformQuad t bq = fmap c bq
    where c pt = convertToPoint pt *! t 

boundingBoxToQuad :: GMBB -> GMBQ
boundingBoxToQuad bb = bq
    where
        V2 (V2 minX minY) (V2 maxX maxY) = bb
        r1 = V2 minX minY
        r2 = V2 maxX minY
        r3 = V2 maxX maxY
        r4 = V2 minX maxY
        bq = V4 r1 r2 r3 r4

quadToBoundingBox :: GMBQ -> GMBB
quadToBoundingBox bq = bb
    where
        xs = fmap (^. _x) bq
        ys = fmap (^. _y) bq
        minX = foldl1 min xs
        minY = foldl1 min ys
        maxX = foldl1 max xs
        maxY = foldl1 max ys
        bb = V2 (V2 minX minY) (V2 maxX maxY)

mergeBB :: GMBB -> GMBB -> GMBB
mergeBB bb1 bb2 = bb3
    where
        V2 (V2 x1 y1) (V2 x2 y2) = bb1
        V2 (V2 x3 y3) (V2 x4 y4) = bb2
        bmin = V2 (min x1 x3) (min y1 y3)
        bmax = V2 (max x2 x4) (max y2 y4)
        bb3 = V2 bmin bmax

pointWithinBB :: GV2D -> GMBB -> Bool
pointWithinBB (V2 x0 y0) (V2 (V2 x1 y1) (V2 x2 y2))
    | x0 < x1 = False
    | x0 > x2 = False
    | y0 < y1 = False
    | y0 > y2 = False
    | otherwise = True


infixl 8 @!*!

(@!*!) :: GM32 -> GM32 -> GM32
m1 @!*! m2 = V3 r1 r2 r3
    where
        (V3 (V2 a b) (V2 c d) (V2 e f)) = m1
        (V3 (V2 g h) (V2 i j) (V2 k l)) = m2
        r1 = V2 (a*g+b*i  ) (a*h+b*j  )
        r2 = V2 (c*g+d*i  ) (c*h+d*j  )
        r3 = V2 (e*g+f*i+k) (e*h+f*j+l)


affineIdentity :: GM32
{-# INLINE affineIdentity #-}
affineIdentity = V3 (unit _x) (unit _y) zero

affineScale :: GV2D
            -> GM32
            -> GM32
{-# INLINE affineScale #-}
affineScale p m = sm !*! m
    where sm = scaled $ convertToPoint p

affineTranslate :: GV2D
                -> GM32
                -> GM32
{-# INLINE affineTranslate #-}
affineTranslate (V2 x y) m = tm !*! m
    where
        -- row major, put x and y on the bottom
        sv = (_x .~ x) . (_y .~ y)
        tm = identity & _z %~ sv

affineRotateRad :: TransformUnit
                -> GM32
                -> GM32
affineRotateRad rad m = tr !*! m
    where
        (cr, sr) = (cos rad, sin rad)
        nsr = (-1) * sr
        svx = _x %~ (_x .~ cr) . (_y .~ sr)
        svy = _y %~ (_x .~ nsr) . (_y .~ cr )
        tr = identity & svx . svy

affineRotateRadOrigin :: TransformUnit -> GM32 -> GM32
affineRotateRadOrigin rad m
    = affineTranslate zero
    $ affineRotateRad rad
    $ affineTranslate zero m

{-
[  a  b  0 ] -1   [  1  0  0 ]                  [  d -b  0 ]
[  c  d  0 ]    = [  0  1  0 ] * (1 / (ad - bc))[ -c  a  0 ]
[  x  y  1 ]      [ -x -y  1 ]                  [  0  0  1 ]
or
[ (d / (ad-bc))     (b / (bc-ad))       0]
[ (c / (bc-ad))     (a / (ad-bc))       0]
[ ((dx-cy)/(bc-ad)) ((bx-ay)/(ad-bc))   1]
-}

affineInverse :: GM32 -> GM32
affineInverse m = im
    where
        V3 (V2 a b) (V2 c d) (V2 x y) = m
        (ad,bc,dx,cy,bx,ay) = (a*d,b*c,d*x,c*y,b*x,a*y)
        (adbc, bcad) = (ad - bc, bc - ad)
        (dxcy, bxay) = (dx - cy, bx - ay)
        r1 = V2 (d    / adbc) (b    / bcad)
        r2 = V2 (c    / bcad) (a    / adbc)
        r3 = V2 (dxcy / bcad) (bxay / adbc)
        im = if adbc == 0 then zero else V3 r1 r2 r3

--