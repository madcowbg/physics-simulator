-----------------------------------------------------------------------------
--
-- Module      :  Physics.Forces
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

module Physics.Forces (
    Gravity (Gravity),
    BouncingGround (BouncingGround),
    StickingGround (StickingGround),
    AirResistance (AirResistance)
) where


import Physics.Primitives
import Physics.AbstractObjects
import Physics.AbstractForces
import Physics.Time

data Gravity = Gravity {accel :: Accelleration, gconst :: Double}

instance Force Gravity where
    act force tick globalPlace globalVel obj
                    = ForceAction globalPlace (accellGravity force tick (objMass obj))

accellGravity              :: Gravity -> Tick -> Double -> ForceAmt
accellGravity gravity (Tick s) mass = vectorScale (accel gravity) (s * gconst gravity * mass)

data BouncingGround = BouncingGround {zlim :: Double}
instance ShockForce BouncingGround where
    shock (BouncingGround zlim) tick globalPlace obj
                    = if zcoord globalPlace < zlim
                        then ShockAction (mirrorZpos globalPlace zlim) mirrorZvel -- FIXME somewhat wrong ... regarding aggregation - should be craft force
                        else NoShockAction

data StickingGround = StickingGround {zground :: Double}
instance ShockForce StickingGround where
    shock (StickingGround zground) tick globalPlace obj
                    = if zcoord globalPlace < zground
                        then ShockAction (mirrorZpos globalPlace zground) stickZvel -- FIXME somewhat wrong ... regarding aggregation - should be craft force
                        else NoShockAction

data AirResistance = AirResistance {drag :: Double}
instance Force AirResistance where
    act (AirResistance drag) (Tick s) globalPlace globalVel obj
                    = ForceAction globalPlace (vectorScale globalVel (-1 * s * drag))


