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
    Craft, craftPlace, craftRotation, craftMass, move, twist, craftActions, executeForces, partsActions, craftMassCenter, momentOfInertia

) where

import Physics.Primitives
import Physics.AbstractObjects
import Physics.AbstractForces
import Physics.Time

class (Movable c, Rotatable c, ShockableObj c, Accelleratable c) => Craft c where
    craftMass           :: c -> Double
    craftMassCenter     :: c -> Place
    craftPlace          :: c -> PlaceState
    craftRotation       :: c -> RotationState
    momentOfInertia     :: c -> AngularMomentum
    partsActions        :: Tick -> c -> [ForceAction]
    craftActions        :: Tick -> c -> [ShockAction]

    executeForces       :: Tick -> c -> c
    executeForces t c           = executeActions (craftActions t c) (partsActions t c) c

executeActions      :: (Craft c) => [ShockAction] -> [ForceAction] -> c -> c
executeActions shocks actions craft
                            = let mass = craftMass craft in accellerate (apply shocks actions mass) craft -- TODO twist...

apply               :: [ShockAction] -> [ForceAction] -> Double -> Velocity -> Velocity
apply shocks actions mass velocity
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

