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
import Physics.Coordinates.FrameOfReference
import Physics.Coordinates.Rotating
import Physics.AbstractForces
import Physics.Time
import Physics.Forces
import Physics.Craft.Basic

data RigidCraft          = RigidCraft {parts :: [RigidPointObj], coordinates :: RotatingCoordinates, ground :: BouncingGround}

createRigid             :: [RigidPointObj] -> RotatingCoordinates -> BouncingGround -> RigidCraft
createRigid p c g       = centerCraft (RigidCraft p c g)



instance ShockableObj RigidCraft where
    objPlace craft              = placeFrom (coordinates craft) origin

instance Craft RigidCraft where
    massiveParts                = parts

    craftActions craft t        = [shock (ground craft) t (stateFrom (coordinates craft) (StateTriplet origin atrest)) craft]

    craftCoordinates            = coordinates

    moveParts craft diff        = craft {parts = map (moveRigidPointObj diff) (parts craft)}
    changeCoordinates craft f   = craft {coordinates = f (coordinates craft)}


--
--instance Acceleratable RigidCraft where
--    changeVelocity f craft          = craft { coordinates = changeVelocity f (coordinates craft)}
--
--instance Torqueable RigidCraft where
--    changeAngularVelocity f craft   = craft { coordinates = changeAngularVelocity f (coordinates craft)}

instance Rotatable RigidCraft where
    changeOrientation tick craft         = craft { coordinates = changeOrientation tick (coordinates craft)}

instance Movable RigidCraft where
    changePosition tick craft         = craft { coordinates = changePosition tick (coordinates craft)}



