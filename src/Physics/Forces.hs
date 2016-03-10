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


import Physics.Elementary
import Physics.Primitives
import Physics.Coordinates
import Physics.AbstractForces
import Physics.Time

data Gravity = Gravity {gravityDirection :: Acceleration, gravityConstant :: Double}

instance Force Gravity where
    act force tick system localPlace localVelocity obj
                    = ForceAction place (accellGravity force tick (objMass obj))
                    where place = globalPlace system localPlace

accellGravity              :: Gravity -> Tick -> Double -> ForceAmount
accellGravity gravity (Tick s) mass = scaleForceAmt (gravityDirection gravity) (s * gravityConstant gravity * mass)

data BouncingGround = BouncingGround {zlim :: Double}
instance ShockForce BouncingGround where
    shock (BouncingGround zlim) tick system localPlace localVelocity obj
                    = let place = globalPlace system localPlace
                       in if zcoord place < zlim
                        then ShockAction (mirrorZpos place zlim) mirrorZvel
                        else NoShockAction

data StickingGround = StickingGround {zground :: Double}
instance ShockForce StickingGround where
    shock (StickingGround zground) tick  system localPlace localVelocity obj
                    = let place = globalPlace system localPlace
                       in if zcoord place < zground
                        then ShockAction (mirrorZpos place zground) stickZvel
                        else NoShockAction

data AirResistance = AirResistance {drag :: Double}
instance Force AirResistance where
    act (AirResistance drag) (Tick s) system localPlace localVelocity obj
                    = ForceAction place (scaleForceAmt velocity (-1 * s * drag))
                     where (place, velocity) = globalState system (localPlace, localVelocity)
