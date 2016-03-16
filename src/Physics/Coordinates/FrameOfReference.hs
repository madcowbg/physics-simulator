-----------------------------------------------------------------------------
--
-- Module      :  Physics.Coordinates.FrameOfReference
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

module Physics.Coordinates.FrameOfReference (
    StateTriplet (StateTriplet),
    FrameOfReference, zeroLocation, zeroOrientation, systemVelocity,
    EmbeddedFrameOfReference, placeFrom, placeTo, velocityFrom, velocityTo, stateFrom, stateTo, accelerationFrom, accelerationTo, directionFrom,
) where
import Physics.Elementary
import Physics.Primitives


data StateTriplet = StateTriplet {locationInFrame :: Place, velocityInFrame :: Velocity}

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
    velocityTo              :: f -> Place -> Velocity -> Velocity

    accelerationFrom        :: f -> Acceleration -> Acceleration
    accelerationFrom system = orientVector (zeroOrientation system)

    accelerationTo          :: f -> Acceleration -> Acceleration
    accelerationTo system   = reverseOrientVector (zeroOrientation system)

    stateFrom               :: f -> StateTriplet -> StateTriplet
    stateFrom system (StateTriplet localPlace localVelocity)
                                = let parentPlace = placeFrom system localPlace
                                  in StateTriplet parentPlace (velocityFrom system localPlace localVelocity)

    stateTo                 :: f -> StateTriplet -> StateTriplet
    stateTo system (StateTriplet parentPlace parentVelocity)
                                = let childPlace = placeTo system parentPlace
                                  in StateTriplet childPlace (velocityTo system childPlace parentVelocity)

    directionFrom           :: f -> Place -> Place
    directionFrom system    = orientVector (zeroOrientation system)

