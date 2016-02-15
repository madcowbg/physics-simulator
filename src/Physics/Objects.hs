-----------------------------------------------------------------------------
--
-- Module      :  Physics.Objects
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
module Physics.Objects (
    Craft, craftMass, move, twist,
    craftActions, executeForces, partsActions, momentOfInertia, craftCoordinates,
    shockCraft

) where

import Physics.Coordinates
import Physics.Elementary
import Physics.AbstractForces
import Physics.Time

class (Movable c, Rotatable c, ShockableObj c, Accelleratable c, Torqueable c) => Craft c where
    craftMass           :: c -> Double
    momentOfInertia     :: c -> MomentOfInertia

    craftCoordinates    :: c -> CoordinateSystem
    partsActions        :: Tick -> c -> [ForceAction]
    craftActions        :: Tick -> c -> [ShockAction]

    executeForces       :: Tick -> c -> c
    executeForces t c           = executeActions (craftActions t c) (partsActions t c) c

    shockCraft          :: [ShockAction] -> c -> c

executeActions      :: (Craft c) => [ShockAction] -> [ForceAction] -> c -> c
executeActions shocks actions craft
                            = shockCraft shocks
                            $ accellerate (applyActions actions (craftMass craft))
                            $ torque (aggregateTorqueForce (map (localActions (craftCoordinates craft)) actions) (momentOfInertia craft) ) craft

applyActions        :: [ForceAction] -> Double -> Accelleration
applyActions actions mass       = sumForceAmt (map (actionToAccelleration mass) actions)

actionToAccelleration             :: Double -> ForceAction -> Accelleration
actionToAccelleration mass (ForceAction place forceAmt)  = scaleAccelleration forceAmt (1 / mass)

localActions        :: CoordinateSystem -> ForceAction -> ForceAction
localActions system (ForceAction globalPlace forceAmt)
                    = ForceAction (localPlace system globalPlace) (localAccelleration system forceAmt)

aggregateTorqueForce     :: [ForceAction] -> MomentOfInertia -> Torque
aggregateTorqueForce forces mom =  aggregateTorque (map (`applyTorque` mom) forces)

applyTorque         :: ForceAction -> MomentOfInertia -> Torque
applyTorque (ForceAction actionPlace forceAmt)
                    =  calcTorque forceAmt actionPlace


