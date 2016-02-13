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
    act force tick change craftVel obj
                    = ForceAction (change (objPlace obj)) (accellGravity force tick (objMass obj))

accellGravity              :: Gravity -> Tick -> Double -> ForceAmt
accellGravity gravity (Tick s) mass = vectorScale (accel gravity) (s * gconst gravity * mass)

data BouncingGround = BouncingGround {zlim :: Double}
instance ShockForce BouncingGround where
    shock (BouncingGround zlim) tick craftPlace obj
                    = let actionPlace = (craftPlace + objPlace obj)
                        in if zcoord actionPlace < zlim
                        then ShockAction (mirrorZpos actionPlace zlim) mirrorZvel -- FIXME somewhat wrong ... regarding aggregation - should be craft force
                        else NoShockAction

data StickingGround = StickingGround {zground :: Double}
instance ShockForce StickingGround where
    shock (StickingGround zground) tick craftPlace obj
                    = let actionPlace = (craftPlace + objPlace obj)
                        in if zcoord actionPlace < zground
                        then ShockAction (mirrorZpos actionPlace zground) stickZvel -- FIXME somewhat wrong ... regarding aggregation - should be craft force
                        else NoShockAction

data AirResistance = AirResistance {drag :: Double}
instance Force AirResistance where
    act (AirResistance drag) (Tick s) change craftVel obj
                    = ForceAction (change (objPlace obj)) (vectorScale craftVel (-1 * s * drag))


