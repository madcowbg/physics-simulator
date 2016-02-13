-----------------------------------------------------------------------------
--
-- Module      :  Physics.Craft.Rigid
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

module Physics.Craft.Rigid (
    RigidCraft (RigidCraft),
    RigidPointObj (RigidPointObj)
) where
import Physics.Primitives
import Physics.AbstractObjects
import Physics.AbstractForces
import Physics.Time
import Physics.Forces
import Physics.Objects

--class (Craft c) => SteerableCraft c where
--     numControls :: c -> Int
--     applyControls :: [Command] -> c -> c
--
--type Command = Thruster -> Thruster
--
----data Rocket = Rocket {masses :: [PhysicalObj], thrusters :: [Thrusters]
--data Thruster p = Thruster {obj :: RigidPointObj, thrust :: Orientation}


---------------------------
data RigidCraft          = RigidCraft {parts :: [RigidPointObj], placeState :: PlaceState, rotationState :: RotationState, ground :: BouncingGround}

instance ShockableObj RigidCraft where
    objPlace                    = place . craftPlace

instance Craft RigidCraft where
    craftMass craft             = sum (map objMass (parts craft))
    craftMassCenter craft       =  sum (map (\obj -> vectorScale (objPlace obj) (objMass obj)) (parts craft))
    craftPlace                  = placeState
    craftRotation               = rotationState
    momentOfInertia c           = makevect 1 1 1 -- FIXME calculate properly!

    partsActions t c
                                = concatMap (\p -> actOnChain (forces p) t (objPlace c) (velocity (placeState c)) p) (parts c)
    craftActions t c            = [shock (ground c) t (objPlace c) c]

instance Accelleratable RigidCraft where
    accellerate f craft         = craft { placeState = accellerate f (craftPlace craft)}


instance Rotatable RigidCraft where
    twist tick craft         = RigidCraft (parts craft) (craftPlace craft) (twist tick (craftRotation craft)) (ground craft)

instance Movable RigidCraft where
    move tick craft         = RigidCraft (parts craft) (move tick (craftPlace craft)) (craftRotation craft) (ground craft)

data RigidPointObj    = RigidPointObj {localPlace :: Place, mass :: Double, forces :: ForceChain}

instance ShockableObj RigidPointObj where
    objPlace            = localPlace

instance PhysicalObj RigidPointObj where
    objMass             = mass


