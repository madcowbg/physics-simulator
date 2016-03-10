-----------------------------------------------------------------------------
--
-- Module      :  Physics.Elementary
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

module Physics.Elementary (
    Vector3,
    Place,
    Velocity,
    Acceleration,
    ForceAmount,
    InertiaTensor,
    makevect,
    identityOrient,
    Orientation, Spin,
    AngularVelocity, AngularAcceleration,
    AngularMomentum, Torque,
    coerceVersor,
) where

import Linear

type Vector3        = V3 Double
type Matrix33       = M33 Double
type Quaternion4    =Quaternion Double

-- place & direction primitives
type Place          = Vector3
type Velocity       = Vector3

type Acceleration   = Vector3
type Torque         = Vector3
type ForceAmount    = Vector3

type Orientation    = Quaternion4
type Spin           = Quaternion4
type AngularVelocity= Vector3
type AngularAcceleration = Vector3
type AngularMomentum= Vector3

type InertiaTensor  = Matrix33

makevect            = V3

identityOrient      :: Quaternion4
identityOrient      = coerceVersor (Quaternion 1 (V3 0 0 0))

coerceVersor        :: Quaternion4 -> Quaternion4
coerceVersor v@(Quaternion w vec)
                    = v ^/ norm v
