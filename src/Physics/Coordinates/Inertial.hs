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
    FrameOfReference, zeroLocation, zeroOrientation, systemVelocity,
    StateTriplet (StateTriplet),
    InertialCoordinates (InertialCoordinates), location, velocity, orientation,
    Movable, changePosition,
    EmbeddedFrameOfReference, toParentPlace, toChildPlace, toParentVelocity, toChildVelocity
) where

import Physics.Elementary
import Physics.Time
import Physics.Primitives


data (FrameOfReference f) => StateTriplet f = StateTriplet {locationInFrame :: Place, velocityInFrame :: Velocity, frame :: f}

class FrameOfReference s where
    zeroLocation :: s -> Place
    zeroOrientation :: s -> Orientation
    systemVelocity    :: s -> Velocity

class (FrameOfReference f) => EmbeddedFrameOfReference f where
    toParentPlace               :: f -> Place -> Place
    toParentPlace system place  = orientVector (zeroOrientation system) place + zeroLocation system

    toChildPlace                :: f -> Place -> Place
    toChildPlace system place   = reverseOrientVector (zeroOrientation system) (zeroLocation system - place)

    toParentVelocity            :: f -> Place -> Velocity -> Velocity
    toChildVelocity             :: f -> Place -> Velocity -> Velocity

data InertialCoordinates = InertialCoordinates {location :: Place, velocity :: Velocity, orientation :: Orientation}

instance FrameOfReference InertialCoordinates where
    zeroLocation                = location
    zeroOrientation             = orientation
    systemVelocity              = velocity

instance EmbeddedFrameOfReference InertialCoordinates where
    toParentVelocity system localPlace localVel
                                = systemVelocity system + orientVector (zeroOrientation system) localVel

    toChildVelocity system childPlace parentVel
                                = reverseOrientVector (zeroOrientation system) (parentVel - systemVelocity system)
class Movable m where
    changePosition          :: Tick -> m -> m
