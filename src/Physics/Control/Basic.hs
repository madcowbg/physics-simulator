-----------------------------------------------------------------------------
--
-- Module      :  Physics.Control.Basic
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

module Physics.Control.Basic (
    ControlledCraft, thrusters, currentControls, applyControls,
    Thruster(Thruster), moveThruster, actThrust, applyControl,
    Control (Control),
    ControlStrategy (ControlStrategy,  NoStrategy), controlSequence,
    ControlState (ControlState), steerCraft,
    ThrusterForce (ThrusterForce),
    -- TODO hide
    maxPower, percentThrust, thrustDirection,
) where
import Physics.Coordinates
import Physics.Elementary
import Physics.AbstractForces
import Physics.Time
import Physics.Craft.Basic

data Thruster = Thruster {percentThrust :: Double, forcePlace :: Place, force :: ThrusterForce}
instance PhysicalObj Thruster where
    objMass o           = 0
instance ShockableObj Thruster where
    objPlace            = forcePlace

moveThruster            :: Place -> Thruster -> Thruster
moveThruster diff p     = p  {forcePlace = forcePlace p + diff}



data ThrusterForce = ThrusterForce {maxPower :: Double, thrustDirection :: Accelleration}

actThrust                   :: Tick -> CoordinateSystem -> Thruster -> Velocity -> ForceAction
actThrust (Tick s) coordinates thruster localVel
                            = ForceAction (globalPlace coordinates (objPlace thruster))
                          (scaleForceAmt (globalAccelleration coordinates (thrustDirection thrusterForce)) (-s * maxPower thrusterForce * percentThrust thruster))
                            where thrusterForce = force thruster

class (Craft cc) => ControlledCraft cc where
     thrusters              :: cc -> [Thruster]
     currentControls        :: cc -> [Control]
     currentControls hull  = map (Control . percentThrust) (thrusters hull)

     applyControls          :: cc -> [Control] -> cc

applyControl                :: Control -> Thruster -> Thruster
applyControl control thruster
                            = thruster {percentThrust = thrustLevel control}


data Control = Control {thrustLevel :: Double}
data ControlStrategy = ControlStrategy {controlSequence :: [(Double, ControlState)] } | NoStrategy
data ControlState = ControlState { thrustersState :: [Control]}

steerCraft                  :: (ControlledCraft cc) => ControlState -> cc -> cc
steerCraft state craft      = applyControls craft (thrustersState state)


