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
    RotatingCoordinates (GlobalSystem, RotatingCoordinates),

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

import Physics.Coordinates.FrameOfReference
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

instance EmbeddedFrameOfReference RotatingCoordinates where
    velocityFrom GlobalSystem _ _ = error "cannot find parent of global system"
    velocityFrom system localPlace localVel
                                = systemVelocity system
                                  + orientVector (zeroOrientation system) (localVel + rotationVelocity (angularVelocity system) localPlace)

    velocityTo GlobalSystem _ _ = error "cannot find parent of global system"
    velocityTo system childPlace parentVel
                                = parentVel - systemVelocity system
                                  - orientVector (zeroOrientation system) (rotationVelocity (angularVelocity system) childPlace)


setPlaceAndUpdateVelocity   :: RotatingCoordinates -> Place -> (Velocity -> Velocity) -> RotatingCoordinates
setPlaceAndUpdateVelocity system place fVel = system {inertial = (inertial system) {location = place, velocity = fVel (systemVelocity system)}}

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

-- TODO distribute those below to separate classes
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
