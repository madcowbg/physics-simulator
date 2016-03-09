-----------------------------------------------------------------------------
--
-- Module      :  Physics.Craft.Rocket
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

module Physics.Craft.Rocket (
    Rocket (Rocket), hull, rocketThrusters,
    Thruster (Thruster),
) where
import Physics.Coordinates
import Physics.Elementary
import Physics.Craft.Basic
import Physics.Craft.Rigid
import Physics.AbstractForces
import Physics.Time

import Physics.Control.Basic

data Rocket = Rocket {hull :: RigidCraft, rocketThrusters :: [Thruster]}

instance Craft Rocket where
    massiveParts                = massiveParts . hull -- TODO add thrusters

    partsActions rocket tick    = partsActions (hull rocket) tick ++ map (thrustAction tick rocket) (thrusters rocket)
    craftActions                = craftActions . hull

    shockCraft actions rocket   = rocket {hull = shockCraft actions (hull rocket)}
    craftCoordinates            = craftCoordinates . hull

    moveParts rocket diff       = rocket {hull = moveParts (hull rocket) diff, rocketThrusters = map (moveThruster diff) (rocketThrusters rocket)} -- TODO add thrusters
    changeCoordinates rocket f  = rocket {hull = changeCoordinates (hull rocket) f}


thrustAction                :: Tick -> Rocket -> Thruster -> ForceAction
thrustAction tick rocket thruster    = actThrust tick (craftCoordinates rocket) thruster atrest


instance Acceleratable Rocket where
    accelerate f rocket    = rocket { hull = accelerate f (hull rocket)}

instance Torqueable Rocket where
    torque f rocket         = rocket { hull = torque f (hull rocket)}

instance Rotatable Rocket where
    twist tick rocket       = rocket { hull = twist tick (hull rocket)}

instance Movable Rocket where
    move tick rocket        = rocket { hull = move tick (hull rocket)}

instance ShockableObj Rocket where
    objPlace rocket         = objPlace (hull rocket)




instance ControlledCraft Rocket where
    thrusters               = rocketThrusters
    applyControls rocket controls
                            = rocket { rocketThrusters = zipWith applyControl controls (rocketThrusters rocket)}


