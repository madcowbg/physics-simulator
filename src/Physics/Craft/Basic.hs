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
    Craft,
    craftActions, executeForces, partsActions, inertiaTensor, craftCoordinates,
    shockCraft, calculateMassCenter, massiveParts, moveParts, centerCraft, changeCoordinates,
    RigidPointObj (RigidPointObj), moveRigidPointObj,
    changeOrientation, changePosition
) where

import Physics.Coordinates.Inertial
import Physics.Coordinates.Rotating
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

    mass                :: c -> Double
    mass craft          = sum (map objMass (massiveParts craft))

    inertiaTensor       :: c -> InertiaTensor
    inertiaTensor craft = calculateIntertiaTensor (map (\p -> (objPlace p, objMass p)) (massiveParts craft))

    craftCoordinates    :: c -> RotatingCoordinates

    partsActions        :: c -> Tick -> [ForceAction]
    partsActions craft t        = concatMap (\p -> actOnChain (forces p) t (globalReference craft p) p) (massiveParts craft)

    craftActions        :: c -> Tick -> [ShockAction]

    executeForces       :: Tick -> c -> c
    executeForces t c           = executeActions (craftActions c t) (partsActions c t) c

    shockCraft          :: [ShockAction] -> c -> c
    shockCraft shocks craft = changeCoordinates craft (coordinatesShock shocks)

    calculateMassCenter         :: c -> Place
    calculateMassCenter craft       = calculateCenterMass (map (\p -> (objPlace p, objMass p)) (massiveParts craft)) (mass craft)

    centerCraft         :: c -> c
    centerCraft craft   = moveParts craft (- calculateMassCenter craft)

    moveParts           :: c -> Place -> c

    changeCoordinates   :: c -> (RotatingCoordinates -> RotatingCoordinates) -> c

globalReference     :: (Craft c) => c -> RigidPointObj -> StateTriplet RotatingCoordinates
globalReference craft p
                    = globalState (StateTriplet (objPlace p) atrest (craftCoordinates craft))

executeActions      :: (Craft c) => [ShockAction] -> [ForceAction] -> c -> c
executeActions shocks actions craft
                            = (shockCraft shocks . accelerate system aggregateForce . spin system aggregateTorque) craft
                              where (aggregateForce, aggregateTorque) = aggregateActions (map (localAction system) actions)
                                    system = craftCoordinates craft

accelerate          :: (Craft c) => RotatingCoordinates -> ForceAmount -> c -> c
accelerate system f c      = changeCoordinates c (changeVelocity (globalAcceleration system (asAcceleration f (mass c))))

spin              :: (Craft c) => RotatingCoordinates -> Torque -> c -> c
spin system torque c     = changeCoordinates c (changeAngularVelocity (globalAcceleration system (asAngularAcceleration torque (inertiaTensor c))))

aggregateActions    :: [ForceAction] -> (ForceAmount, Torque)
aggregateActions actions
                    = (sumForcesAmount (map forceAmount actions), sumForcesAmount (map forceTorque actions))

forceAmount         :: ForceAction -> ForceAmount
forceAmount (ForceAction place forceAmt) = forceAmt

forceTorque         :: ForceAction -> Torque
forceTorque (ForceAction place amt)
                        =  calcTorque amt place

localAction         :: RotatingCoordinates -> ForceAction -> ForceAction
localAction system (ForceAction globalPlace forceAmt)
                    = ForceAction (localPlace system globalPlace) (localAcceleration system forceAmt)

coordinatesShock                :: [ShockAction] -> RotatingCoordinates -> RotatingCoordinates
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

