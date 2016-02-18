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
    Rocket (Rocket),
    Thruster (Thruster),
    ThrusterForce (ThrusterForce), maxPower, percentThrust, thrustDirection,
) where
import Physics.Coordinates
import Physics.Elementary
import Physics.Objects
import Physics.Craft.Rigid
import Physics.AbstractForces
import Physics.Time


data Rocket = Rocket {craft :: RigidCraft, thrusters :: [Thruster]}

instance Craft Rocket where
    massiveParts                = massiveParts . craft -- TODO add thrusters

    partsActions rocket tick    = partsActions (craft rocket) tick ++ map (thrustAction tick rocket) (thrusters rocket)
    craftActions                = craftActions . craft

    shockCraft actions rocket   = rocket {craft = shockCraft actions (craft rocket)}
    craftCoordinates            = craftCoordinates . craft

    moveParts rocket diff       = rocket {craft = moveParts (craft rocket) diff} -- TODO add thrusters
    changeCoordinates rocket f  = rocket {craft = changeCoordinates (craft rocket) f}


thrustAction                :: Tick -> Rocket -> Thruster -> ForceAction
thrustAction tick rocket thruster    = act (force thruster) tick (craftCoordinates rocket) (forcePlace thruster) atrest thruster

data Thruster = Thruster {forcePlace :: Place, force :: ThrusterForce}
instance PhysicalObj Thruster where
    objMass o           = 0
instance ShockableObj Thruster where
    objPlace            = forcePlace


data ThrusterForce = ThrusterForce {maxPower :: Double, percentThrust :: Double, thrustDirection :: Accelleration}
instance Force ThrusterForce where
    act force (Tick s) coordinates localPlace localVel obj
            = ForceAction (globalPlace coordinates localPlace)
                          (scaleForceAmt (globalAccelleration coordinates (thrustDirection force)) (-s * maxPower force * percentThrust force))

instance Accelleratable Rocket where
    accellerate f rocket        = rocket { craft = accellerate f (craft rocket)}

instance Torqueable Rocket where
    torque f rocket             = rocket { craft = torque f (craft rocket)}

instance Rotatable Rocket where
    twist tick rocket        = rocket { craft = twist tick (craft rocket)}

instance Movable Rocket where
    move tick rocket        = rocket { craft = move tick (craft rocket)}

instance ShockableObj Rocket where
    objPlace rocket              = objPlace (craft rocket)


--class (Craft c) => SteerableCraft c where
--     numControls :: c -> Int
--     applyControls :: [Command] -> c -> c
--
--type Command = Thruster -> Thruster
--


---------------------------
