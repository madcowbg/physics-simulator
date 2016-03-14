-----------------------------------------------------------------------------
--
-- Module      :  Physics.Energy
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

module Physics.Energy (
    potentialEnergy,
    calcPotential,
    calcKinetic,
) where

import Physics.Coordinates
import Physics.Elementary
import Physics.AbstractForces
import Physics.Forces
import Physics.Craft.Basic
import Physics.Craft.Rocket

class PotentialForce f where
    potentialEnergy     :: f -> RotatingCoordinates -> RigidPointObj -> Double

instance PotentialForce Gravity where
    potentialEnergy force system obj
                        = -scalarProduct (gravityDirection force) place * gravityConstant force * objMass obj
                            where place = globalPlace system (objPlace obj)

class HasEnergy o where
    calcPotential      :: (PotentialForce f, HasEnergy o) => f -> o -> Double
    calcKinetic      :: (HasEnergy o) => o -> Double

instance HasEnergy Rocket where
    calcPotential force rocket = sum (map (potentialEnergy force (craftCoordinates rocket)) (massiveParts rocket))
    calcKinetic rocket = sum (map (kineticEnergy (craftCoordinates rocket)) (massiveParts rocket))

--instance HasEnergy RigidPointObj where
--    calcEnergy force o = potentialEnergy force

kineticEnergy       :: RotatingCoordinates -> RigidPointObj -> Double
kineticEnergy system obj = let (StateTriplet _ velocity _) = globalState (StateTriplet (objPlace obj) atrest system) in scalarProduct velocity velocity * objMass obj / 2
