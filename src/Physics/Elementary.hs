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
    Accelleration,
    ForceAmt,
    Orientation,
    Rotation (Rotation), Torque,
    MomentOfInertia,
    makevect,
    identityOrient,
) where

import Linear.V3
import Linear.Matrix

type Vector3        = V3 Double
type Matrix33       = M33 Double

-- place & direction primitives
type Place          = Vector3
type Velocity       = Vector3

type Accelleration  = Vector3
type ForceAmt       = Vector3

type Orientation    = Matrix33

data Rotation       = Rotation {xEffect, yEffect, zEffect :: Double}
type MomentOfInertia= Rotation
type Torque         = Rotation

makevect            = V3

identityOrient      :: Matrix33
identityOrient      = V3 (V3 1 0 0)
                         (V3 0 1 0)
                         (V3 0 0 1)
