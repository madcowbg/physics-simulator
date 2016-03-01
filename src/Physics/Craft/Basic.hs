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
    Craft, craftMass, move, twist,
    craftActions, executeForces, partsActions, momentOfInertia, craftCoordinates,
    shockCraft, calculateMassCenter, massiveParts, moveParts, centerCraft, changeCoordinates,
    RigidPointObj (RigidPointObj), moveRigidPointObj
) where

import Physics.Coordinates
import Physics.Elementary
import Physics.AbstractForces
import Physics.Time

class (Movable c, Rotatable c, ShockableObj c, Accelleratable c, Torqueable c) => Craft c where
    massiveParts            :: c -> [RigidPointObj]

    craftMass               :: c -> Double
    craftMass craft         = sum (map objMass (massiveParts craft))
    momentOfInertia         :: c -> InertiaMatrix
    momentOfInertia craft       = calculateIntertiaMatrix (map (\p -> (objPlace p, objMass p)) (massiveParts craft))

    craftCoordinates    :: c -> CoordinateSystem

    partsActions        :: c -> Tick -> [ForceAction]
    partsActions craft t        = concatMap (\p -> actOnChain (forces p) t (craftCoordinates craft) (objPlace p) atrest p) (massiveParts craft)

    craftActions        :: c -> Tick -> [ShockAction]

    executeForces       :: Tick -> c -> c
    executeForces t c           = executeActions (craftActions c t) (partsActions c t) c

    shockCraft          :: [ShockAction] -> c -> c
    shockCraft shocks craft = changeCoordinates craft (coordinatesShock shocks)

    calculateMassCenter         :: c -> Place
    calculateMassCenter craft       = calculateCenterMass (map (\p -> (objPlace p, mass p)) (massiveParts craft)) (craftMass craft)

    centerCraft         :: c -> c
    centerCraft craft   = moveParts craft (- calculateMassCenter craft)

    moveParts           :: c -> Place -> c

    changeCoordinates   :: c -> (CoordinateSystem -> CoordinateSystem) -> c



executeActions      :: (Craft c) => [ShockAction] -> [ForceAction] -> c -> c
executeActions shocks actions craft
                            = shockCraft shocks
                            $ accellerate (applyActions actions (craftMass craft))
                            $ torque (aggregateTorqueForce actions (craftCoordinates craft) (momentOfInertia craft) )
                            $ craft

applyActions        :: [ForceAction] -> Double -> Accelleration
applyActions actions mass       = sumForceAmt (map (actionToAccelleration mass) actions)

actionToAccelleration             :: Double -> ForceAction -> Accelleration
actionToAccelleration mass (ForceAction place forceAmt)  = scaleAccelleration forceAmt (1 / mass)

localActions        :: CoordinateSystem -> ForceAction -> ForceAction
localActions system (ForceAction globalPlace forceAmt)
                    = ForceAction (localPlace system globalPlace) (localAccelleration system forceAmt)

aggregateTorqueForce     :: [ForceAction] -> CoordinateSystem -> InertiaMatrix -> AngularAcceleration
aggregateTorqueForce forces system inertiaMatrix
                        = globalAccelleration system (calculateAngularAcceleration (aggregateLocalTorqueForce system forces) inertiaMatrix)

aggregateLocalTorqueForce:: CoordinateSystem -> [ForceAction] -> Torque
aggregateLocalTorqueForce system forces
                        = sum (map (calcForceTorque . localActions system) forces)

calcForceTorque         :: ForceAction -> Torque
calcForceTorque (ForceAction place amt)
                        =   calcTorque place amt

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


data RigidPointObj    = RigidPointObj {place :: Place, mass :: Double, forces :: ForceChain}

instance ShockableObj RigidPointObj where
    objPlace            = place

instance PhysicalObj RigidPointObj where
    objMass             = mass


moveRigidPointObj           :: Place -> RigidPointObj -> RigidPointObj
moveRigidPointObj diff p    = p  {place = place p + diff}

