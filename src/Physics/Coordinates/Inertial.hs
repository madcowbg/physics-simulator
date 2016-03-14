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

) where

import Physics.Elementary
import Physics.Time


data (FrameOfReference f) => StateTriplet f = StateTriplet {locationInFrame :: Place, velocityInFrame :: Velocity, frame :: f}

class FrameOfReference s where
    zeroLocation :: s -> Place
    zeroOrientation :: s -> Orientation
    systemVelocity    :: s -> Velocity


data InertialCoordinates = InertialCoordinates {location :: Place, velocity :: Velocity, orientation :: Orientation}

instance FrameOfReference InertialCoordinates where
    zeroLocation                = location
    zeroOrientation             = orientation
    systemVelocity              = velocity

class Movable m where
    changePosition          :: Tick -> m -> m
