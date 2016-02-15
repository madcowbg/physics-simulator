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
    localAccelleration,
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

    --------
    aggregateTorque, calcTorque,
    calculateMomentOfIntertia,
    calculateCenterMass,
    -------- possibly not ok?
    setPlaceAndUpdateVelocity,

) where

import Physics.Primitives
import Physics.Elementary
import Physics.Time

data CoordinateSystem = GlobalSystem | CoordinateSystem {parent :: CoordinateSystem,
                                        zeroLocation :: Place, velocity :: Velocity, orientation :: Orientation, rotation :: Rotation}

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
toParentVelocity system place vel
                            = velocity system + vel + calcRotationVelocity place (orientation system) (rotation system)

toChildVelocity             :: CoordinateSystem -> Place -> Velocity -> Velocity
toChildVelocity GlobalSystem _ _ = error "cannot find parent of global system"
toChildVelocity system place vel
                            = vel - velocity system - calcRotationVelocity place (orientation system) (rotation system)

globalState                 :: CoordinateSystem -> (Place, Velocity) -> (Place, Velocity)
globalState                 = toGlobal parent (\system pv -> (toParentPlace system (fst pv), toParentVelocity system (fst pv) (snd pv)))

localState                  :: CoordinateSystem -> (Place, Velocity) -> (Place, Velocity)
localState                  = toLocal parent (\system pv -> (toChildPlace system (fst pv), toChildVelocity system (fst pv) (snd pv)))

-- TODO write in analytic form
deltaNumericalApprox = 0.01
calcRotationVelocity            :: Place -> Orientation -> Rotation -> Velocity
calcRotationVelocity place systemOrientation systemRotation
                                = vectorScale (orientVector deltaRotation place - orientVector systemOrientation place) (1/deltaNumericalApprox)
                                  where deltaRotation = rotateOrientation systemOrientation systemRotation deltaNumericalApprox


class Movable m where
    move                :: Tick -> m -> m

class Accelleratable a where
    accellerate         :: Accelleration -> a -> a

class Rotatable r where
    twist              :: Tick -> r -> r

class Torqueable t where
    torque              :: Torque -> t -> t


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
    torque torque system= system {rotation = aggregateTorque [rotation system, torque]}

sumForceAmt         = vectorSum
scaleForceAmt       = vectorScale
scaleAccelleration  = vectorScale

origin              = makevect 0.0 0.0 0.0
atrest              = makevect 0.0 0.0 0.0

aggregateTorque     = torqueSum

calculateCenterMass         :: [(Place, Double)] -> Double -> Place
calculateCenterMass pts mass= vectorSum (map (\p -> vectorScale (fst p) (snd p / mass)) pts)

calculateMomentOfIntertia   :: [(Place, Double)] -> MomentOfInertia
calculateMomentOfIntertia massPoints = aggregateTorque (map neededTorque massPoints)

neededTorque        :: (Place, Double) -> Torque
neededTorque (placePart, massPart)
                    = calculateRotationIntertia direction massPart
                    where direction = placePart
