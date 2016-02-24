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
    ThrusterForce (ThrusterForce), maxPower, percentThrust, thrustDirection,
    Control (Control),
    ControlStrategy (ControlStrategy,  NoStrategy), controlSequence,
    ControlState (ControlState),
    ControlledCraft, steerCraft,
) where
import Physics.Coordinates
import Physics.Elementary
import Physics.Objects
import Physics.Craft.Rigid
import Physics.AbstractForces
import Physics.Time


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
thrustAction tick rocket thruster    = actThrust (force thruster) tick (craftCoordinates rocket) (forcePlace thruster) atrest thruster

data Thruster = Thruster {percentThrust :: Double, forcePlace :: Place, force :: ThrusterForce}
instance PhysicalObj Thruster where
    objMass o           = 0
instance ShockableObj Thruster where
    objPlace            = forcePlace

moveThruster            :: Place -> Thruster -> Thruster
moveThruster diff p     = p  {forcePlace = forcePlace p + diff}



data ThrusterForce = ThrusterForce {maxPower :: Double, thrustDirection :: Accelleration}

actThrust                   :: ThrusterForce -> Tick -> CoordinateSystem -> Place -> Velocity -> Thruster -> ForceAction
actThrust force (Tick s) coordinates localPlace localVel thruster
                            = ForceAction (globalPlace coordinates localPlace)
                          (scaleForceAmt (globalAccelleration coordinates (thrustDirection force)) (-s * maxPower force * percentThrust thruster))

instance Accelleratable Rocket where
    accellerate f rocket    = rocket { hull = accellerate f (hull rocket)}

instance Torqueable Rocket where
    torque f rocket         = rocket { hull = torque f (hull rocket)}

instance Rotatable Rocket where
    twist tick rocket       = rocket { hull = twist tick (hull rocket)}

instance Movable Rocket where
    move tick rocket        = rocket { hull = move tick (hull rocket)}

instance ShockableObj Rocket where
    objPlace rocket         = objPlace (hull rocket)


class (Craft cc) => ControlledCraft cc where
     thrusters              :: cc -> [Thruster]
     currentControls        :: cc -> [Control]
     currentControls hull  = map (Control . percentThrust) (thrusters hull)

     applyControls          :: cc -> [Control] -> cc

instance ControlledCraft Rocket where
    thrusters               = rocketThrusters
    applyControls rocket controls
                            = rocket { rocketThrusters = zipWith applyControl controls (rocketThrusters rocket)}

applyControl                :: Control -> Thruster -> Thruster
applyControl control thruster
                            = thruster {percentThrust = thrustLevel control}


data Control = Control {thrustLevel :: Double}
data ControlStrategy = ControlStrategy {controlSequence :: [(Double, ControlState)] } | NoStrategy
data ControlState = ControlState { thrustersState :: [Control]}

steerCraft                  :: (ControlledCraft cc) => ControlState -> cc -> cc
steerCraft state craft      = applyControls craft (thrustersState state)



