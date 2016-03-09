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
    Place,
    Velocity,
    Acceleration,
    ForceAmt,
    Orientation,
    --Rotation (Rotation), Torque,
    InertiaMatrix,
    makevect,
    identityOrient,
    AngularVelocity,
    AngularAcceleration,
    RotationMatrix,
    Torque
) where

import Linear.V3
import Linear.Matrix

type Vector3        = V3 Double
type Matrix33       = M33 Double

-- place & direction primitives
type Place          = Vector3
type Velocity       = Vector3

type Acceleration  = Vector3
type ForceAmt       = Vector3

type Orientation    = Matrix33

type Torque         = Vector3
type RotationMatrix = Matrix33
type InertiaMatrix  = Matrix33
type AngularVelocity= Vector3
type AngularAcceleration
                    = Vector3

makevect            = V3

identityOrient      :: Matrix33
identityOrient      = V3 (V3 1 0 0)
                         (V3 0 1 0)
                         (V3 0 0 1)
