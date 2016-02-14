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
data RigidCraft          = RigidCraft {parts :: [RigidPointObj], coordinates :: CoordinateSystem, ground :: BouncingGround}

instance ShockableObj RigidCraft where
    objPlace craft              = globalPlace (coordinates craft) origin

instance Craft RigidCraft where
    craftMass craft             = sum (map objMass (parts craft))
    --craftMassCenter craft       =  sum (map (\obj -> vectorScale (objPlace obj) (objMass obj / craftMass craft)) (parts craft))
    --craftPlace                  = placeState
    --craftRotation               = rotationState
    momentOfInertia craft       = torqueSum (map (neededTorque origin) (parts craft))

    partsActions t craft        = concatMap (\p -> actOnChain (forces p) t (globalState (coordinates craft) (objPlace p, atrest)) p) (parts craft)
    craftActions t c            = [shock (ground c) t (objPlace c) c]

    shockCraft shocks craft     = craft { coordinates = coordinatesShock shocks (coordinates craft)}
    craftCoordinates            = coordinates

--applyShocks shocks (
--(coordinates { PlaceState (place (placeState craft))
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


--globalPosition      :: RigidCraft -> RigidPointObj -> Place
--globalPosition craft obj = objPlace craft + orientVector (orient (rotationState craft)) (objPlace obj)

--globalVelocity      :: RigidCraft -> RigidPointObj -> Tick -> Place
--globalVelocity craft obj tick = velocity (placeState craft) + calcRotationVelocity (objPlace obj - craftMassCenter craft) (rotationState craft) tick


-- TODO move to a more primitive import...
-- TODO write in analytic form
--calcRotationVelocity
--calcRotationVelocity            :: Place -> RotationState -> Tick -> Velocity
--calcRotationVelocity localPlace origRotation tick@(Tick s)
--                                = vectorScale (orientVector (orient deltaRotation) localPlace - orientVector (orient origRotation) localPlace) (1/s)
--                                  where deltaRotation = twist tick origRotation

neededTorque        :: Place -> RigidPointObj -> Torque
neededTorque centerMass (RigidPointObj placePart massPart _)
                    = calculateRotationIntertia direction massPart
                    where direction = placePart - centerMass

instance Accelleratable RigidCraft where
    accellerate f craft         = craft { coordinates = accellerate f (coordinates craft)}

instance Torqueable RigidCraft where
    torque f craft              = craft { coordinates = torque f (coordinates craft)}

instance Rotatable RigidCraft where
    twist tick craft         = craft { coordinates = twist tick (coordinates craft)}

instance Movable RigidCraft where
    move tick craft         = craft { coordinates = move tick (coordinates craft)}

data RigidPointObj    = RigidPointObj {localPlace :: Place, mass :: Double, forces :: ForceChain}

instance ShockableObj RigidPointObj where
    objPlace            = Physics.Craft.Rigid.localPlace

instance PhysicalObj RigidPointObj where
    objMass             = mass


