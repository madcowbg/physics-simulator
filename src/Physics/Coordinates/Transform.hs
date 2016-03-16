-----------------------------------------------------------------------------
--
-- Module      :  Physics.Coordinates.Transform
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

module Physics.Coordinates.Transform (
--    globalPlace, localPlace,
--    localAcceleration, globalAcceleration,
--    globalState, localState,
--    globalOrientation,
) where

import Physics.Coordinates.FrameOfReference



class (EmbeddedFrameOfReference f) => NestedFrameOfReference f where
    transformPlaceFrom      ::



--    globalPlace                 :: e -> Place -> Place
--    localPlace                  :: e -> Place -> Place
--
--    -- accelleration is just rotated
--    localAcceleration          :: e -> Acceleration -> Acceleration
--
--    globalAcceleration         :: e -> Acceleration -> Acceleration
--    globalOrientation          :: e -> Place -> Place
--
--
--    globalState                 :: StateTriplet e -> StateTriplet e
--    localState                  :: StateTriplet e -> StateTriplet e

instance EmbeddedNonInertialFrameOfReference RotatingCoordinates where
--    globalPlace                 = toGlobal parent placeFrom
--    globalAcceleration          = toGlobal parent (\system accel -> orientVector (zeroOrientation system) accel)
--    globalOrientation           = toGlobal parent (\system orient -> orientVector (zeroOrientation system) orient)
--    globalState state@(StateTriplet _ _ frame)
--                                = toGlobal parent stateFrom frame state
--
--    localPlace                  = toLocal parent placeTo
--    localAcceleration           = toLocal parent (\system accel -> reverseOrientVector (zeroOrientation system) accel)
--    localState state@(StateTriplet _ _ frame)
--                                = toLocal parent stateTo frame state


toLocal                        :: (RotatingCoordinates -> RotatingCoordinates) -> (RotatingCoordinates -> a -> a) -> RotatingCoordinates -> a -> a
toLocal f g GlobalSystem val   = val
toLocal f g system place       = g system (toLocal f g (f system) place)

toGlobal                      :: (RotatingCoordinates -> RotatingCoordinates) -> (RotatingCoordinates -> a -> a) -> RotatingCoordinates -> a -> a
toGlobal f g GlobalSystem val = val
toGlobal f g system val       = toGlobal f g (f system) (g system val)

