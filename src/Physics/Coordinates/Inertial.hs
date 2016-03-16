-----------------------------------------------------------------------------
--
-- Module      :  Coordinates.Inertial
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

module Physics.Coordinates.Inertial (
    InertialCoordinates (InertialCoordinates), location, velocity, orientation,
    Movable, changePosition,
) where

import Physics.Elementary
import Physics.Time
import Physics.Primitives
import Physics.Coordinates.FrameOfReference


data InertialCoordinates = InertialCoordinates {location :: Place, velocity :: Velocity, orientation :: Orientation}

instance FrameOfReference InertialCoordinates where
    zeroLocation                = location
    zeroOrientation             = orientation
    systemVelocity              = velocity

instance EmbeddedFrameOfReference InertialCoordinates where
    velocityFrom system localPlace localVel
                                = systemVelocity system + orientVector (zeroOrientation system) localVel

    velocityTo system childPlace parentVel
                                = reverseOrientVector (zeroOrientation system) (parentVel - systemVelocity system)
class Movable m where
    changePosition          :: Tick -> m -> m
