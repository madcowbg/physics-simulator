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
{-# LANGUAGE ExistentialQuantification #-}

module Physics.Objects (
    Craft, craftPlace, craftRotation, craftMass, move, twist, craftActions, executeForces, partsActions, craftMassCenter, momentOfInertia, relativeToCraft

) where

import Physics.Primitives
import Physics.AbstractObjects
import Physics.AbstractForces
import Physics.Time

class (Movable c, Rotatable c, ShockableObj c, Accelleratable c, Torqueable c) => Craft c where
    craftMass           :: c -> Double
    momentOfInertia     :: c -> MomentOfInertia

    craftMassCenter     :: c -> Place
    craftPlace          :: c -> PlaceState
    craftRotation       :: c -> RotationState
    partsActions        :: Tick -> c -> [ForceAction]
    craftActions        :: Tick -> c -> [ShockAction]

    relativeToCraft     :: c -> Place -> Place
    relativeToCraft c p  =  place (craftPlace c) + orientVector (orient (craftRotation c)) p

    executeForces       :: Tick -> c -> c
    executeForces t c           = executeActions (craftActions t c) (partsActions t c) c

executeActions      :: (Craft c) => [ShockAction] -> [ForceAction] -> c -> c
executeActions shocks actions craft
                            = accellerate (aggregateAccelleration shocks actions (craftMass craft))
                            $ torque (aggregateTorque (map (toLocalView (craftPlace craft) (craftMassCenter craft) (craftRotation craft)) actions) (momentOfInertia craft) ) craft

aggregateAccelleration               :: [ShockAction] -> [ForceAction] -> Double -> Velocity -> Velocity
aggregateAccelleration shocks actions mass velocity
                    = applyShocks shocks (applyActions actions mass velocity)

applyShocks         :: [ShockAction] -> Velocity -> Velocity
applyShocks         = foldr ((.) . applyShock) id

applyShock                          :: ShockAction -> Velocity -> Velocity
applyShock NoShockAction            = id
applyShock (ShockAction place fun)  = fun

applyActions        :: [ForceAction] -> Double -> Velocity -> Velocity
applyActions actions mass velocity = vectorSum (velocity:map (actionToAccelleration mass) actions)

actionToAccelleration             :: Double -> ForceAction -> Accelleration
actionToAccelleration mass (ForceAction place forceAmt)  = vectorScale forceAmt (1 / mass)

toLocalView         :: PlaceState -> Place -> RotationState -> ForceAction -> ForceAction
toLocalView (PlaceState craftPlace _) craftMassCenter (RotationState orientation _) (ForceAction globalPlace forceAmt)
                    = ForceAction (globalPlace - craftPlace - craftMassCenter) (reverseOrientVector orientation forceAmt)

aggregateTorque     :: [ForceAction] -> MomentOfInertia -> Rotation -> Rotation
aggregateTorque forces mom rot =  torqueSum (rot:map (`applyTorque` mom) forces)

applyTorque         :: ForceAction -> MomentOfInertia -> Torque
applyTorque (ForceAction actionPlace forceAmt)
                    =  calcTorque forceAmt actionPlace    -- must be -force, because of reasons?


