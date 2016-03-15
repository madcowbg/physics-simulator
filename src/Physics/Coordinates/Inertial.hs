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
    EmbeddedFrameOfReference, placeFrom, placeTo, velocityFrom, velocityTo, stateFrom, stateTo,
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
    placeFrom               :: f -> Place -> Place
    placeFrom system place  = orientVector (zeroOrientation system) place + zeroLocation system

    placeTo                 :: f -> Place -> Place
    placeTo system place    = reverseOrientVector (zeroOrientation system) (zeroLocation system - place)

    velocityFrom            :: f -> Place -> Velocity -> Velocity
    velocityTo             :: f -> Place -> Velocity -> Velocity

    stateFrom               :: f -> StateTriplet f -> StateTriplet f
    stateFrom system (StateTriplet localPlace localVelocity frame)
                                = let parentPlace = placeFrom system localPlace
                                  in StateTriplet parentPlace (velocityFrom system localPlace localVelocity) system

    stateTo                :: f -> StateTriplet f -> StateTriplet f
    stateTo system (StateTriplet parentPlace parentVelocity frame)
                                = let childPlace = placeTo system parentPlace
                                  in StateTriplet childPlace (velocityTo system childPlace parentVelocity) system

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
