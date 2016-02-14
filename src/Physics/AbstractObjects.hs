-----------------------------------------------------------------------------
--
-- Module      :  Physics.AbstractObjects
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

module Physics.AbstractObjects (
    PlaceState (PlaceState), place, velocity,
    RotationState (RotationState), orient, rotation,
    ShockableObj, objPlace,
    PhysicalObj, objMass,

) where
import Physics.Primitives
import Physics.Time

class ShockableObj o where
    objPlace        :: o -> Place

--data PhysicalObj    = PhysicalObj {localPlace :: Place, mass :: Double}
class (ShockableObj o) => PhysicalObj o where
    objMass             :: o -> Double

-- States
data PlaceState     = PlaceState {place :: Place, velocity :: Velocity}
data RotationState  = RotationState {orient:: Orientation, rotation :: Rotation}

instance Movable PlaceState where
    move (Tick s) (PlaceState place velocity)
                        = PlaceState (vectorMulAdd place velocity s) velocity

instance Accelleratable PlaceState where
    accellerate f state = state {velocity = f + velocity state}

instance Rotatable RotationState where
    twist (Tick s) (RotationState orient rotation)
                        = RotationState (rotateOrientation orient rotation s) rotation

instance Torqueable RotationState where
    torque f state      = state {rotation = torqueSum [f, rotation state]}
