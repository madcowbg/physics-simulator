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
    parentFrameOfReference :: (EmbeddedFrameOfReference g) => f -> g
    hasParent               :: f -> Bool

    transformPlaceFrom      :: (EmbeddedFrameOfReference g) => g -> f -> Place
    transformPlaceFrom      = transform placeFrom placeTo

    transformStateFrom      :: (EmbeddedFrameOfReference g) => g -> f -> StateTriplet
    transformStateFrom      = transform stateFrom stateTo

    transformPlaceTo        :: (EmbeddedFrameOfReference g) => f -> g -> Place


transform   :: (EmbeddedFrameOfReference f, EmbeddedFrameOfReference g) => (f -> a) -> (g -> a) -> f -> g -> a -> a
transform fwd back f g param
            = let commonParent = findCommonParent g f
               in transformFromParent back f commonParent . transformToParent fwd g commonParent
            -- g --(back)--> commonParent --(forward)--> f

transformToParent   :: (EmbeddedFrameOfReference f, EmbeddedFrameOfReference g) => (g -> a) -> f -> g -> a -> a
transformToParent fwd g commonParent
                    | f == parent       = param
                    | otherwise         = transformToParent fwd (parentFrameOfReference f) parent (fwd param)

transformFromParent :: (EmbeddedFrameOfReference f, EmbeddedFrameOfReference g) => (f -> a) -> f -> g -> a -> a
transformFromParent back f commonParent
                    | f == parent       = param
                    | otherwise         = back (transformToParent back (parentFrameOfReference f) parent param)

findCommonParent    :: (EmbeddedFrameOfReference f, EmbeddedFrameOfReference g, EmbeddedFrameOfReference u) => f -> g -> u
findCommonParent f g= parentFrameOfReference (removeCommon (parents f) (parents g))

removeCommon        :: (EmbeddedFrameOfReference f) => [f] -> [g] -> f
removeCommon [] _   = error
removeCommon _  []  = error
removeCommon (f:~fs) (g:~gs)
                    | f == g    = removeCommon fs gs
                    | otherwise = f

parents             :: (EmbeddedFrameOfReference f) => f -> [f]
parents f           | hasParent f   = f:parents (parentFrameOfReference f)
                    | otherwise     = [f]

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

