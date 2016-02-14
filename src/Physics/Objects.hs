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
    Craft, craftPlace, craftRotation, craftMass, move, twist,
    craftActions, executeForces, partsActions, craftMassCenter, momentOfInertia, relativeToCraft,
    shockCraft

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

    shockCraft          :: [ShockAction] -> c -> c

executeActions      :: (Craft c) => [ShockAction] -> [ForceAction] -> c -> c
executeActions shocks actions craft
                            = shockCraft shocks
                            $ accellerate (applyActions actions (craftMass craft))
                            $ torque (aggregateTorque (map (toLocalView (craftPlace craft) (craftMassCenter craft) (craftRotation craft)) actions) (momentOfInertia craft) ) craft

--aggregateAccelleration               :: [ShockAction] -> [ForceAction] -> Double -> Accelleration
--aggregateAccelleration shocks actions mass
--                    = applyActions actions mass

applyActions        :: [ForceAction] -> Double -> Accelleration
applyActions actions mass       = vectorSum (map (actionToAccelleration mass) actions)

actionToAccelleration             :: Double -> ForceAction -> Accelleration
actionToAccelleration mass (ForceAction place forceAmt)  = vectorScale forceAmt (1 / mass)

toLocalView         :: PlaceState -> Place -> RotationState -> ForceAction -> ForceAction
toLocalView (PlaceState craftPlace _) craftMassCenter (RotationState orientation _) (ForceAction globalPlace forceAmt)
                    = ForceAction (globalPlace - craftPlace - craftMassCenter) (reverseOrientVector orientation forceAmt)

aggregateTorque     :: [ForceAction] -> MomentOfInertia -> Torque
aggregateTorque forces mom =  torqueSum (map (`applyTorque` mom) forces)

applyTorque         :: ForceAction -> MomentOfInertia -> Torque
applyTorque (ForceAction actionPlace forceAmt)
                    =  calcTorque forceAmt actionPlace    -- must be -force, because of reasons?


