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
    Craft, craftPlace, craftMass, move, rotate, craftActions, executeActions, executeForces, partsActions,
    RigidCraft (RigidCraft),
    RigidPointObj (RigidPointObj)
) where

import Physics.Primitives
import Physics.AbstractObjects
import Physics.AbstractForces
import Physics.Time
import Physics.Forces

--data Rocket = Rocket {masses :: [PhysicalObj], thrusters :: [Thrusters]
data Thruster = Thruster {obj :: RigidPointObj, thrust :: Orientation}

data RigidCraft          = RigidCraft {parts :: [RigidPointObj], placeState :: PlaceState, rotationState :: RotationState, ground :: BouncingGround}
data RigidPointObj    = RigidPointObj {localPlace :: Place, mass :: Double, forces :: ForceChain}



class (Craft c) => SteerableCraft c where
     numControls :: c -> Int
     applyControls :: [Command] -> c -> c


type Command = Thruster -> Thruster

class (Movable c, Rotatable c, ShockableObj c, Accelleratable c) => Craft c where
    craftMass           :: c -> Double
    craftMassCenter     :: c -> Place
    craftPlace          :: c -> PlaceState
    craftRotation       :: c -> RotationState
    angularMomentum     :: c -> AngularMomentum
    partsActions        :: Tick -> c -> [ForceAction]
    craftActions        :: Tick -> c -> [ShockAction]

    executeForces       :: Tick -> c -> c

instance ShockableObj RigidCraft where
    objPlace                    = place . craftPlace

instance Craft RigidCraft where
    craftMass craft             = sum (map objMass (parts craft))
    craftMassCenter craft       =  sum (map (\obj -> vectorScale (objPlace obj) (objMass obj)) (parts craft))
    craftPlace                  = placeState
    craftRotation               = rotationState
    angularMomentum c           = makevect 1 1 1 -- FIXME calculate properly!

    partsActions t c
                                = concatMap (\p -> actOnChain (forces p) t (objPlace c) (velocity (placeState c)) p) (parts c)
    craftActions t c            = [shock (ground c) t (objPlace c) c]

    executeForces t c           = executeActions (craftActions t c) (partsActions t c) c

instance Accelleratable RigidCraft where
    accellerate f craft         = craft { placeState = accellerate f (craftPlace craft)}

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


instance Rotatable RigidCraft where
    rotate tick craft         = RigidCraft (parts craft) (craftPlace craft) (rotate tick (craftRotation craft)) (ground craft)

instance Movable RigidCraft where
    move tick craft         = RigidCraft (parts craft) (move tick (craftPlace craft)) (craftRotation craft) (ground craft)

-- Particle State
instance ShockableObj RigidPointObj where
    objPlace            = localPlace

instance PhysicalObj RigidPointObj where
    objMass             = mass


