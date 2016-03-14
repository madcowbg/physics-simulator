-----------------------------------------------------------------------------
--
-- Module      :  Physics.Coordinates.Rotating
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

module Physics.Coordinates.Rotating (
    EmbeddedNonInertialFrameOfReference,
    InertialCoordinates (InertialCoordinates),
    RotatingCoordinates (GlobalSystem, RotatingCoordinates),
    globalPlace, localPlace,
    localAcceleration, globalAcceleration,
    globalState, localState,
    globalOrientation,
    --------
    Rotatable, changeOrientation,
    Movable, changePosition,
    Acceleratable, changeVelocity,
    Torqueable, changeAngularVelocity,
    origin, atrest, identityOrient,
    --------
    sumForcesAmount,
    scaleForceAmount,
    scaleAccelleration,
    distance,
    --------
    calculateIntertiaTensor,
    calculateCenterMass,
    -------- possibly not ok?
    setPlaceAndUpdateVelocity,
    scalarProduct,
    ---------
    asAcceleration,
    asAngularAcceleration,
    calcTorque,
) where

import Physics.Primitives

import Physics.Coordinates.Inertial
import Physics.Elementary
import Physics.Time


class (FrameOfReference s) => NonInertialFrameOfReference s where
    systemAngularVelocity :: s -> AngularVelocity

data RotatingCoordinates = GlobalSystem | RotatingCoordinates {parent :: RotatingCoordinates, inertial :: InertialCoordinates, angularVelocity :: AngularVelocity}

instance FrameOfReference RotatingCoordinates where
    zeroLocation                = location . inertial
    zeroOrientation             = orientation . inertial
    systemVelocity              = velocity . inertial

instance NonInertialFrameOfReference RotatingCoordinates where
    systemAngularVelocity       = angularVelocity

class (NonInertialFrameOfReference e) => EmbeddedNonInertialFrameOfReference e where
    globalPlace                 :: e -> Place -> Place
    localPlace                  :: e -> Place -> Place

    -- accelleration is just rotated
    localAcceleration          :: e -> Acceleration -> Acceleration

    globalAcceleration         :: e -> Acceleration -> Acceleration
    globalOrientation          :: e -> Place -> Place


    globalState                 :: StateTriplet e -> StateTriplet e
    localState                  :: StateTriplet e -> StateTriplet e


instance EmbeddedFrameOfReference RotatingCoordinates where
    toParentVelocity GlobalSystem _ _ = error "cannot find parent of global system"
    toParentVelocity system localPlace localVel
                                = systemVelocity system
                                  + orientVector (zeroOrientation system) (localVel + rotationVelocity (angularVelocity system) localPlace)

    toChildVelocity GlobalSystem _ _ = error "cannot find parent of global system"
    toChildVelocity system childPlace parentVel
                                = parentVel - systemVelocity system
                                  - orientVector (zeroOrientation system) (rotationVelocity (angularVelocity system) childPlace)

instance EmbeddedNonInertialFrameOfReference RotatingCoordinates where
    globalPlace                 = toGlobal parent toParentPlace
    globalAcceleration          = toGlobal parent (\system accel -> orientVector (zeroOrientation system) accel)
    globalOrientation           = toGlobal parent (\system orient -> orientVector (zeroOrientation system) orient)
    globalState state@(StateTriplet _ _ frame)
                                = toGlobal parent toParentState frame state

    localPlace                  = toLocal parent toChildPlace
    localAcceleration           = toLocal parent (\system accel -> reverseOrientVector (zeroOrientation system) accel)
    localState state@(StateTriplet _ _ frame)
                                = toLocal parent toChildState frame state


setPlaceAndUpdateVelocity   :: RotatingCoordinates -> Place -> (Velocity -> Velocity) -> RotatingCoordinates
setPlaceAndUpdateVelocity system place fVel = system {inertial = (inertial system) {location = place, velocity = fVel (systemVelocity system)}}

toParentState               :: RotatingCoordinates -> StateTriplet RotatingCoordinates -> StateTriplet RotatingCoordinates
toParentState system (StateTriplet localPlace localVelocity frame)
                            = let parentPlace = toParentPlace system localPlace
                              in StateTriplet parentPlace (toParentVelocity system localPlace localVelocity) system

toChildState                :: RotatingCoordinates -> StateTriplet RotatingCoordinates -> StateTriplet RotatingCoordinates
toChildState system (StateTriplet parentPlace parentVelocity frame)
                            = let childPlace = toChildPlace system parentPlace
                              in StateTriplet childPlace (toChildVelocity system childPlace parentVelocity) system

toLocal                        :: (RotatingCoordinates -> RotatingCoordinates) -> (RotatingCoordinates -> a -> a) -> RotatingCoordinates -> a -> a
toLocal f g GlobalSystem val   = val
toLocal f g system place       = g system (toLocal f g (f system) place)

toGlobal                      :: (RotatingCoordinates -> RotatingCoordinates) -> (RotatingCoordinates -> a -> a) -> RotatingCoordinates -> a -> a
toGlobal f g GlobalSystem val = val
toGlobal f g system val       = toGlobal f g (f system) (g system val)


class Rotatable r where
    changeOrientation       :: Tick -> r -> r

class Acceleratable a where
    changeVelocity          :: Acceleration -> a -> a

class Torqueable t where
    changeAngularVelocity   :: AngularAcceleration -> t -> t

instance Movable RotatingCoordinates where
    changePosition (Tick dt) system
                        = system {inertial = (inertial system) {location = integratePosition (zeroLocation system) (systemVelocity system) dt}}

instance Rotatable RotatingCoordinates where
    changeOrientation (Tick dt) system
                        = system {inertial = (inertial system) {orientation = integrateOrientation (zeroOrientation system) (angularVelocity system) dt}}

instance Acceleratable RotatingCoordinates where
    changeVelocity acceleration system
                        = system {inertial = (inertial system) {velocity = integrateVelocity (systemVelocity system) acceleration}}

instance Torqueable RotatingCoordinates where
    changeAngularVelocity angularAcceleration system
                        = system {angularVelocity = integrateAngularVelocity (systemAngularVelocity system) angularAcceleration}

sumForcesAmount     = vectorSum
scaleForceAmount    = vectorScale
scaleAccelleration  = vectorScale

scalarProduct       :: Vector3 -> Vector3 -> Double
scalarProduct       = dot

distance            :: Vector3 -> Double
distance            = norm

origin              = makevect 0.0 0.0 0.0
atrest              = makevect 0.0 0.0 0.0

calculateCenterMass         :: [(Place, Double)] -> Double -> Place
calculateCenterMass pts mass= vectorSum (map (\p -> vectorScale (fst p) (snd p / mass)) pts)

calculateIntertiaTensor   :: [(Place, Double)] -> InertiaTensor
calculateIntertiaTensor massPoints = sum (map inertiaTensorComponent massPoints)
