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
    RigidPointObj (RigidPointObj),
    createRigid
) where
import Physics.Elementary
import Physics.Coordinates
import Physics.AbstractForces
import Physics.Time
import Physics.Forces
import Physics.Objects

data RigidCraft          = RigidCraft {parts :: [RigidPointObj], coordinates :: CoordinateSystem, ground :: BouncingGround}

createRigid             :: [RigidPointObj] -> CoordinateSystem -> BouncingGround -> RigidCraft
createRigid p c g       = centerRigid (RigidCraft p c g)

centerRigid             :: RigidCraft -> RigidCraft
centerRigid craft       = let massCenter = calculateCenterMass (map (\p -> (objPlace p, mass p)) (parts craft)) (craftMass craft)
                            in craft {parts = map (\part -> part { place = place part - massCenter }) (parts craft)}

instance ShockableObj RigidCraft where
    objPlace craft              = globalPlace (coordinates craft) origin

instance Craft RigidCraft where
    craftMass craft             = sum (map objMass (parts craft))
    momentOfInertia craft       = calculateMomentOfIntertia (map (\p -> (objPlace p, mass p)) (parts craft))

    partsActions t craft        = concatMap (\p -> actOnChain (forces p) t (coordinates craft) (objPlace p) atrest p) (parts craft)
    craftActions t craft        = [shock (ground craft) t (coordinates craft) origin atrest craft]

    shockCraft shocks craft     = craft { coordinates = coordinatesShock shocks (coordinates craft)}
    craftCoordinates            = coordinates

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


instance Accelleratable RigidCraft where
    accellerate f craft         = craft { coordinates = accellerate f (coordinates craft)}

instance Torqueable RigidCraft where
    torque f craft              = craft { coordinates = torque f (coordinates craft)}

instance Rotatable RigidCraft where
    twist tick craft         = craft { coordinates = twist tick (coordinates craft)}

instance Movable RigidCraft where
    move tick craft         = craft { coordinates = move tick (coordinates craft)}

data RigidPointObj    = RigidPointObj {place :: Place, mass :: Double, forces :: ForceChain}

instance ShockableObj RigidPointObj where
    objPlace            = place

instance PhysicalObj RigidPointObj where
    objMass             = mass


