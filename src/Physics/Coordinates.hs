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
    localAccelleration, globalAccelleration,
    globalState, localState,
    globalOrientation,
    --------
    Rotatable, twist,
    Movable, move,
    Accelleratable, accellerate,
    Torqueable, torque,
    origin, atrest, identityOrient,
    --------
    sumForceAmt,
    scaleForceAmt,
    scaleAccelleration,
    distance,
    --------
    calcTorque,
    calculateAngularAcceleration,
    --aggregateTorque, calcTorque,
    --calculateMomentOfIntertia,
    calculateIntertiaMatrix,
    calculateCenterMass,
    -------- possibly not ok?
    setPlaceAndUpdateVelocity,
    scalarProduct,
) where

import Physics.Primitives
import Physics.Elementary
import Physics.Time

data CoordinateSystem = GlobalSystem | CoordinateSystem {parent :: CoordinateSystem,
                                        zeroLocation :: Place, velocity :: Velocity, orientation :: Orientation, rotation :: AngularVelocity}

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
localAccelleration          :: CoordinateSystem -> Accelleration -> Accelleration
localAccelleration          = toLocal parent (\system accel -> reverseOrientVector (orientation system) accel)

globalAccelleration         :: CoordinateSystem -> Accelleration -> Accelleration
globalAccelleration         = toGlobal parent (\system accel -> orientVector (orientation system) accel)

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
                              + orientVector (orientation system) (cross localPlace (rotation system)) 

toChildVelocity             :: CoordinateSystem -> Place -> Velocity -> Velocity
toChildVelocity GlobalSystem _ _ = error "cannot find parent of global system"
toChildVelocity system childPlace parentVel
                            = parentVel - velocity system
                              - orientVector (orientation system)(cross childPlace (rotation system))

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
    move                :: Tick -> m -> m

class Accelleratable a where
    accellerate         :: Accelleration -> a -> a

class Rotatable r where
    twist              :: Tick -> r -> r

class Torqueable t where
    torque              :: AngularAcceleration -> t -> t


instance Movable CoordinateSystem where
    move (Tick s) system
                        = system {zeroLocation = vectorMulAdd (zeroLocation system) (velocity system) s}

instance Accelleratable CoordinateSystem where
    accellerate accelleration system
                        = system {velocity = velocity system + accelleration}

instance Rotatable CoordinateSystem where
    twist (Tick s) system
                        = system {orientation = rotateOrientation (orientation system) (rotation system) s}

instance Torqueable CoordinateSystem where
    torque angularAcceleration system
                        = system {rotation = rotation system + angularAcceleration}

sumForceAmt         = vectorSum
scaleForceAmt       = vectorScale
scaleAccelleration  = vectorScale

scalarProduct       = vdot

distance            = vectorLength

origin              = makevect 0.0 0.0 0.0
atrest              = makevect 0.0 0.0 0.0

calculateCenterMass         :: [(Place, Double)] -> Double -> Place
calculateCenterMass pts mass= vectorSum (map (\p -> vectorScale (fst p) (snd p / mass)) pts)

calculateIntertiaMatrix   :: [(Place, Double)] -> InertiaMatrix
calculateIntertiaMatrix massPoints = sum (map inertiaMatrixComponent massPoints)

calcTorque         :: Place -> ForceAmt -> AngularAcceleration
calcTorque          = cross
