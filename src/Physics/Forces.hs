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
    Force,
    Gravity (Gravity),
    GroundForce (GroundForce),
    ForceAction (ForceAction, ShockAction, NoAction),
    act
) where


import Physics.Primitives
import Physics.Objects

-- Forces
class Force f where
    act                     :: f -> Tick -> Place -> PhysicalObj -> ForceAction

data ForceAction = ForceAction Place ForceAmt | ShockAction Place (Velocity->Velocity) | NoAction

data Gravity = Gravity {accel :: Accelleration, gconst :: Double}

instance Force Gravity where
    act force tick craftPlace (PhysicalObj partPlace mass _)
                    = ForceAction (craftPlace + partPlace) (accellGravity force tick mass)

accellGravity              :: Gravity -> Tick -> Double -> ForceAmt
accellGravity (Gravity direction gconst) (Tick s) mass = vectorScale direction (s * gconst * mass)

data GroundForce = GroundForce {zlim :: Double}
instance Force GroundForce where
    act (GroundForce zlim) tick craftPlace (PhysicalObj partPlace mass _)
                    = let actionPlace = (craftPlace + partPlace)
                        in if zcoord actionPlace < zlim
                        then ShockAction (mirrorZpos actionPlace zlim) mirrorZvel -- FIXME somewhat wrong ... regarding aggregation - should be craft force
                        else NoAction
