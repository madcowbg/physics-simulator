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
    mirrorZvel,
    mirrorZpos,
    stickZvel,

    xcoord,
    ycoord,
    zcoord,

    ------- ok?
    dot, norm,
    orientVector,
    reverseOrientVector,
    vectorScale,
    vectorSum,
    --------
    inertiaTensorComponent,

    integrateOrientation, integrateAngularVelocity,
    integratePosition, integrateVelocity,
    --
    coerceVersor,
    --
    rotationVelocity,
    --
    asAcceleration,
    asAngularAcceleration,
    calcTorque,
) where

import Physics.Elementary
import Linear

type Matrix33       = M33 Double

vectorScale         :: Vector3 -> Double -> Vector3
vectorScale         = (^*)

vectorSum           :: [Vector3] -> Vector3
vectorSum           = foldr (^+^) (makevect 0 0 0)

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


integratePosition       :: Place -> Velocity -> Double -> Place
integratePosition p v dt= p + v ^* dt

integrateVelocity       :: Velocity -> Acceleration -> Velocity
integrateVelocity       = (+)


integrateOrientation    :: Orientation -> AngularVelocity -> Double -> Orientation
integrateOrientation q w dt = coerceVersor (q + qprim q (w ^* dt))

qprim                   :: Orientation -> AngularVelocity -> Spin
qprim q spin            = let spinq = Quaternion 0 spin in 0.5 * (spinq * q)

integrateAngularVelocity    :: AngularVelocity -> AngularAcceleration -> AngularVelocity
integrateAngularVelocity    = (+)

orientVector            :: Orientation -> Vector3 -> Vector3
orientVector            = rotate

reverseOrientVector     :: Orientation -> Vector3 -> Vector3
reverseOrientVector     = rotate . conjugate

inertiaTensorComponent  :: (Place, Double) -> InertiaTensor
inertiaTensorComponent (place, mass)
                    = mass *!! (((norm place ** 2) *!! identity) + outer place place)

rotationVelocity    :: AngularVelocity -> Place -> Velocity
rotationVelocity    = cross


asAcceleration      :: ForceAmount -> Double -> Acceleration
asAcceleration f mass= f ^/ mass

asAngularAcceleration   :: Torque -> InertiaTensor -> AngularAcceleration
asAngularAcceleration torque inertiaTensor
                    =inv33 inertiaTensor !* torque

calcTorque         :: ForceAmount -> Place -> AngularAcceleration
calcTorque          = cross
