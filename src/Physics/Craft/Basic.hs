-----------------------------------------------------------------------------
--
-- Module      :  Physics.Craft.Basic
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

module Physics.Craft.Basic (
    Craft, craftMass,
    craftActions, executeForces, partsActions, inertiaTensor, craftCoordinates,
    shockCraft, calculateMassCenter, massiveParts, moveParts, centerCraft, changeCoordinates,
    RigidPointObj (RigidPointObj), moveRigidPointObj,
    changeOrientation, changePosition,
) where

import Physics.Coordinates
import Physics.Elementary
import Physics.AbstractForces
import Physics.Time


--
--class Massive m where
--    mass                :: m -> Double
--
--class RotatingMassive m where
--    inertiaTensor       :: m -> InertiaTensor
--, Massive c, RotatingMassive c
class (Movable c, Rotatable c, ShockableObj c) => Craft c where
    massiveParts        :: c -> [RigidPointObj]

    craftMass           :: c -> Double
    craftMass craft     = sum (map objMass (massiveParts craft))

    mass                :: c -> Double
    mass                = craftMass

    inertiaTensor       :: c -> InertiaTensor
    inertiaTensor craft = calculateIntertiaMatrix (map (\p -> (objPlace p, objMass p)) (massiveParts craft))

    craftCoordinates    :: c -> CoordinateSystem

    partsActions        :: c -> Tick -> [ForceAction]
    partsActions craft t        = concatMap (\p -> actOnChain (forces p) t (craftCoordinates craft) (objPlace p) atrest p) (massiveParts craft)

    craftActions        :: c -> Tick -> [ShockAction]

    executeForces       :: Tick -> c -> c
    executeForces t c           = executeActions (craftActions c t) (partsActions c t) c

    shockCraft          :: [ShockAction] -> c -> c
    shockCraft shocks craft = changeCoordinates craft (coordinatesShock shocks)

    calculateMassCenter         :: c -> Place
    calculateMassCenter craft       = calculateCenterMass (map (\p -> (objPlace p, objMass p)) (massiveParts craft)) (craftMass craft)

    centerCraft         :: c -> c
    centerCraft craft   = moveParts craft (- calculateMassCenter craft)

    moveParts           :: c -> Place -> c

    changeCoordinates   :: c -> (CoordinateSystem -> CoordinateSystem) -> c



executeActions      :: (Craft c) => [ShockAction] -> [ForceAction] -> c -> c
executeActions shocks actions craft
                            = (shockCraft shocks . accelerate system aggregateForce . spin system aggregateTorque) craft
                              where (aggregateForce, aggregateTorque) = aggregateActions (map (localAction system) actions)
                                    system = craftCoordinates craft
--                            $ accelerate (applyActions actions (craftMass craft))
--                            $ torque (aggregateTorqueForce actions (craftCoordinates craft) (momentOfInertia craft) )
--                            $ craft

accelerate          :: (Craft c) => CoordinateSystem -> ForceAmount -> c -> c
accelerate system f c      = changeCoordinates c (changeVelocity (globalAcceleration system (asAcceleration f (mass c))))

spin              :: (Craft c) => CoordinateSystem -> Torque -> c -> c
spin system torque c     = changeCoordinates c (changeAngularVelocity (globalAcceleration system (asAngularAcceleration torque (inertiaTensor c))))

aggregateActions    :: [ForceAction] -> (ForceAmount, Torque)
aggregateActions actions
                    = (sumForceAmt (map forceAmount actions), sumForceAmt (map forceTorque actions))

--applyActions        :: [ForceAction] -> Double -> Acceleration
--applyActions actions mass       = sumForceAmt (map (actionToAccelleration mass) actions)

forceAmount         :: ForceAction -> ForceAmount
forceAmount (ForceAction place forceAmt) = forceAmt

forceTorque         :: ForceAction -> Torque
forceTorque (ForceAction place amt)
                        =  calcTorque amt place

actionToAccelleration             :: Double -> ForceAction -> Acceleration
actionToAccelleration mass (ForceAction place forceAmt)  = scaleAccelleration forceAmt (1 / mass)

localAction         :: CoordinateSystem -> ForceAction -> ForceAction
localAction system (ForceAction globalPlace forceAmt)
                    = ForceAction (localPlace system globalPlace) (localAcceleration system forceAmt)
--
--aggregateTorqueForce     :: [ForceAction] -> CoordinateSystem -> InertiaTensor -> Spin
--aggregateTorqueForce forces system inertiaMatrix
--                        = globalAcceleration system (calculateSpin (aggregateLocalTorqueForce system forces) inertiaMatrix)
--
--aggregateLocalTorqueForce:: CoordinateSystem -> [ForceAction] -> Torque
--aggregateLocalTorqueForce system forces
--                        = sum (map (calcForceTorque . localActions system) forces)



coordinatesShock                :: [ShockAction] -> CoordinateSystem -> CoordinateSystem
coordinatesShock [] system      = system
coordinatesShock actions@(NoShockAction:as) system
                                = coordinatesShock as system
coordinatesShock actions@(ShockAction place f:as) system
                                = setPlaceAndUpdateVelocity system place (applyShocks actions)

applyShocks         :: [ShockAction] -> Velocity -> Velocity
applyShocks         = foldr ((.) . applyShock) id

applyShock                          :: ShockAction -> Velocity -> Velocity
applyShock NoShockAction            = id
applyShock (ShockAction place fun)  = fun


data RigidPointObj    = RigidPointObj {place :: Place, pointMass :: Double, forces :: ForceChain}

instance ShockableObj RigidPointObj where
    objPlace            = place

instance PhysicalObj RigidPointObj where
    objMass             = pointMass


moveRigidPointObj           :: Place -> RigidPointObj -> RigidPointObj
moveRigidPointObj diff p    = p  {place = place p + diff}

