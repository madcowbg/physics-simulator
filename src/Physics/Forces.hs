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
    Gravity (Gravity), gravityDirection, gravityConstant,
    BouncingGround (BouncingGround),
    StickingGround (StickingGround),
    AirResistance (AirResistance)
) where

import Physics.Coordinates.Inertial
import Physics.Elementary
import Physics.Primitives
import Physics.Coordinates.Rotating
import Physics.AbstractForces
import Physics.Time

data Gravity = Gravity {gravityDirection :: Acceleration, gravityConstant :: Double}

instance Force Gravity where
    action force tick (StateTriplet place _) obj
                    = ForceAction place (accellGravity force tick (objMass obj))

accellGravity              :: Gravity -> Tick -> Double -> ForceAmount
accellGravity gravity (Tick s) mass = scaleForceAmount (gravityDirection gravity) (s * gravityConstant gravity * mass)

data BouncingGround = BouncingGround {zlim :: Double}
instance ShockForce BouncingGround where
    shock (BouncingGround zlim) tick (StateTriplet place _) obj
                    | zcoord place < zlim   = ShockAction (mirrorZpos place zlim) mirrorZvel
                    | otherwise             = NoShockAction

data StickingGround = StickingGround {zground :: Double}
instance ShockForce StickingGround where
    shock (StickingGround zground) tick  (StateTriplet place _) obj
                    | zcoord place < zground= ShockAction (mirrorZpos place zground) stickZvel
                    | otherwise             = NoShockAction

data AirResistance = AirResistance {drag :: Double}
instance Force AirResistance where
    action (AirResistance drag) (Tick s) (StateTriplet place velocity) obj
                    = ForceAction place (scaleForceAmount velocity (-1 * s * drag))
