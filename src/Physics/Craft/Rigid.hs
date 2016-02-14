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
    craftMassCenter craft       =  sum (map (\obj -> vectorScale (objPlace obj) (objMass obj / craftMass craft)) (parts craft))
    craftPlace                  = placeState
    craftRotation               = rotationState
    momentOfInertia craft       = torqueSum (map (neededTorque (craftMassCenter craft)) (parts craft))

    partsActions t c            = concatMap (\p -> actOnChain (forces p) t (globalPosition c p) (globalVelocity c p t) p) (parts c)
    craftActions t c            = [shock (ground c) t (objPlace c) c]

    shockCraft shocks craft     = craft { placeState = PlaceState (place (placeState craft)) (applyShocks shocks (velocity (placeState craft)))}

--applyShocks shocks (
applyShocks         :: [ShockAction] -> Velocity -> Velocity
applyShocks         = foldr ((.) . applyShock) id

applyShock                          :: ShockAction -> Velocity -> Velocity
applyShock NoShockAction            = id
applyShock (ShockAction place fun)  = fun


globalPosition      :: RigidCraft -> RigidPointObj -> Place
globalPosition craft obj = objPlace craft + orientVector (orient (rotationState craft)) (objPlace obj)

globalVelocity      :: RigidCraft -> RigidPointObj -> Tick -> Place
globalVelocity craft obj tick = velocity (placeState craft) + calcRotationVelocity (objPlace obj - craftMassCenter craft) (rotationState craft) tick


-- TODO move to a more primitive import...
-- TODO write in analytic form
--calcRotationVelocity
calcRotationVelocity            :: Place -> RotationState -> Tick -> Velocity
calcRotationVelocity localPlace origRotation tick@(Tick s)
                                = vectorScale (orientVector (orient deltaRotation) localPlace - orientVector (orient origRotation) localPlace) (1/s)
                                  where deltaRotation = twist tick origRotation

neededTorque        :: Place -> RigidPointObj -> Torque
neededTorque centerMass (RigidPointObj placePart massPart _)
                    = calculateRotationIntertia direction massPart
                    where direction = placePart - centerMass

instance Accelleratable RigidCraft where
    accellerate f craft         = craft { placeState = accellerate f (craftPlace craft)}

instance Torqueable RigidCraft where
    torque f craft              = craft { rotationState = torque f (rotationState craft)}

instance Rotatable RigidCraft where
    twist tick craft         = RigidCraft (parts craft) (craftPlace craft) (twist tick (craftRotation craft)) (ground craft)

instance Movable RigidCraft where
    move tick craft         = RigidCraft (parts craft) (move tick (craftPlace craft)) (craftRotation craft) (ground craft)

data RigidPointObj    = RigidPointObj {localPlace :: Place, mass :: Double, forces :: ForceChain}

instance ShockableObj RigidPointObj where
    objPlace            = Physics.Craft.Rigid.localPlace

instance PhysicalObj RigidPointObj where
    objMass             = mass


