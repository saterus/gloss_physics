module Vector
       where

import Graphics.Gloss.Data.Vector

(<+>) :: Vector -> Vector -> Vector
(<+>) = zipV (+)

(<*>) :: Float -> Vector -> Vector
(<*>) = mulSV

(<.>) :: Vector -> Vector -> Float
(<.>) = dotV

(<#>) :: Vector -> Vector -> Float
(<#>) = detV

mapV :: (Float -> Float) -> Vector -> Vector
mapV f (x,y) = ((f x),(f y))

zipV :: (Float -> Float -> Float) -> Vector -> Vector -> Vector
zipV f (x1,y1) (x2,y2) = ((f x1 x2),(f y1 y2))

vZero = (0.0,0.0)
