-----------------------------------------------------------------------------
--
-- Module      :  Physics.Coordinates
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

module Physics.Coordinates (
    CoordinateSystem (GlobalSystem, CoordinateSystem),
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
    sumForceAmt,
    scaleForceAmt,
    scaleAccelleration,
    distance,
    --------
--    calcTorque,
--    calculateAngularAcceleration,
    --aggregateTorque, calcTorque,
    --calculateMomentOfIntertia,
    calculateIntertiaMatrix,
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
import Physics.Elementary
import Physics.Time

data CoordinateSystem = GlobalSystem | CoordinateSystem {parent :: CoordinateSystem,
                                        zeroLocation :: Place, velocity :: Velocity, orientation :: Orientation, angularVelocity :: AngularVelocity}

setPlaceAndUpdateVelocity   :: CoordinateSystem -> Place -> (Velocity -> Velocity) -> CoordinateSystem
setPlaceAndUpdateVelocity system place fVel = system {zeroLocation = place, velocity = fVel (velocity system)}

toParentPlace               :: CoordinateSystem -> Place -> Place
toParentPlace GlobalSystem _= error "cannot find parent of global system"
toParentPlace system place  = orientVector (orientation system) place + zeroLocation system

toChildPlace system place   = reverseOrientVector (orientation system) (zeroLocation system - place)

globalPlace                 :: CoordinateSystem -> Place -> Place
globalPlace                 = toGlobal parent toParentPlace

localPlace                  :: CoordinateSystem -> Place -> Place
localPlace                  = toLocal parent toChildPlace

-- accelleration is just rotated
localAcceleration          :: CoordinateSystem -> Acceleration -> Acceleration
localAcceleration          = toLocal parent (\system accel -> reverseOrientVector (orientation system) accel)

globalAcceleration         :: CoordinateSystem -> Acceleration -> Acceleration
globalAcceleration         = toGlobal parent (\system accel -> orientVector (orientation system) accel)

globalOrientation          :: CoordinateSystem -> Place -> Place
globalOrientation           = toGlobal parent (\system orient -> orientVector (orientation system) orient)

toLocal                        :: (CoordinateSystem -> CoordinateSystem) -> (CoordinateSystem -> a -> a) -> CoordinateSystem -> a -> a
toLocal f g GlobalSystem val   = val
toLocal f g system place       = g system (toLocal f g (f system) place)

toGlobal                      :: (CoordinateSystem -> CoordinateSystem) -> (CoordinateSystem -> a -> a) -> CoordinateSystem -> a -> a
toGlobal f g GlobalSystem val = val
toGlobal f g system val       = toGlobal f g (f system) (g system val)

toParentVelocity            :: CoordinateSystem -> Place -> Velocity -> Velocity
toParentVelocity GlobalSystem _ _ = error "cannot find parent of global system"
toParentVelocity system localPlace localVel
                            = localVel + velocity system
                              + orientVector (orientation system) (rotationVelocity (angularVelocity system) localPlace)
-- orientVector (orientation system) (cross localPlace (rotation system))

toChildVelocity             :: CoordinateSystem -> Place -> Velocity -> Velocity
toChildVelocity GlobalSystem _ _ = error "cannot find parent of global system"
toChildVelocity system childPlace parentVel
                            = parentVel - velocity system
                              - orientVector (orientation system) (rotationVelocity (angularVelocity system) childPlace)
--                              orientVector (orientation system) (cross childPlace (rotation system))

globalState                 :: CoordinateSystem -> (Place, Velocity) -> (Place, Velocity)
globalState                 = toGlobal parent toParentState

toParentState               :: CoordinateSystem -> (Place, Velocity) -> (Place, Velocity)
toParentState system (localPlace, localVelocity)
                            = let parentPlace = toParentPlace system localPlace
                              in (parentPlace, toParentVelocity system localPlace localVelocity)


localState                  :: CoordinateSystem -> (Place, Velocity) -> (Place, Velocity)
localState                  = toLocal parent toChildState

toChildState                :: CoordinateSystem -> (Place, Velocity) -> (Place, Velocity)
toChildState system (parentPlace, parentVelocity)
                            = let childPlace = toChildPlace system parentPlace
                              in (childPlace, toChildVelocity system childPlace parentVelocity)

class Movable m where
    changePosition          :: Tick -> m -> m

class Rotatable r where
    changeOrientation       :: Tick -> r -> r

class Acceleratable a where
    changeVelocity          :: Acceleration -> a -> a

class Torqueable t where
    changeAngularVelocity   :: AngularAcceleration -> t -> t

instance Movable CoordinateSystem where
    changePosition (Tick dt) system
                        = system {zeroLocation = integratePosition (zeroLocation system) (velocity system) dt}

instance Rotatable CoordinateSystem where
    changeOrientation (Tick dt) system
                        = system {orientation = integrateOrientation (orientation system) (angularVelocity system) dt}

instance Acceleratable CoordinateSystem where
    changeVelocity acceleration system
                        = system {velocity = integrateVelocity (velocity system) acceleration}

instance Torqueable CoordinateSystem where
    changeAngularVelocity angularAcceleration system
                        = system {angularVelocity = integrateAngularVelocity (angularVelocity system) angularAcceleration}

sumForceAmt         = vectorSum
scaleForceAmt       = vectorScale
scaleAccelleration  = vectorScale

scalarProduct       = vdot

distance            = vectorLength

origin              = makevect 0.0 0.0 0.0
atrest              = makevect 0.0 0.0 0.0

calculateCenterMass         :: [(Place, Double)] -> Double -> Place
calculateCenterMass pts mass= vectorSum (map (\p -> vectorScale (fst p) (snd p / mass)) pts)

calculateIntertiaMatrix   :: [(Place, Double)] -> InertiaTensor
calculateIntertiaMatrix massPoints = sum (map inertiaMatrixComponent massPoints)

