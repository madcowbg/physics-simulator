-----------------------------------------------------------------------------
--
-- Module      :  Physics.Primitives
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Physics.Primitives (
    Place,
    Velocity,
    Accelleration,
    ForceAmt,
    Orientation,
    Rotation,
    AngularMomentum,
    makevect,
    vectorMulAdd,
    vectorScale,
    vectorSum,
    xcoord,
    ycoord,
    zcoord,
    mirrorZvel,
    mirrorZpos,
    stickZvel
) where

import Linear.V3

type Vector3        = V3 Double

-- place & direction primitives
type Place          = Vector3
type Velocity       = Vector3
type Accelleration  = Vector3
type ForceAmt       = Vector3

type Orientation    = Vector3
type Rotation       = Vector3
type AngularMomentum= Vector3

makevect            = V3

vectorMulAdd        :: Vector3 -> Vector3 -> Double -> Vector3
vectorMulAdd v1 v2 s = v1 + vectorScale v2 s

vectorScale         :: Vector3 -> Double -> Vector3
vectorScale v1 s    = fmap (* s) v1

vectorSum           :: [Vector3] -> Vector3
vectorSum           = foldr (+) (makevect 0 0 0)

xcoord          :: Vector3 -> Double
xcoord (V3 x y z) = x

ycoord          :: Vector3 -> Double
ycoord (V3 x y z) = y

zcoord          :: Vector3 -> Double
zcoord (V3 x y z) = z

mirrorZvel                  :: Velocity -> Velocity
mirrorZvel (V3 x y z)       = V3 x y (-z)

mirrorZpos                  :: Place -> Double -> Place
mirrorZpos (V3 px py pz) z  = V3 px py (z + (z-pz))


stickZvel                  :: Velocity -> Velocity
stickZvel (V3 x y z)       = V3 x y 0


