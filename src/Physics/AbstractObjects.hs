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
    Rotatable, twist,
    Movable, move,
    Accelleratable, accellerate
) where
import Physics.Primitives
import Physics.Time

class ShockableObj o where
    objPlace        :: o -> Place

--data PhysicalObj    = PhysicalObj {localPlace :: Place, mass :: Double}
class (ShockableObj o) => PhysicalObj o where
    objMass             :: o -> Double

class Movable m where
    move                :: Tick -> m -> m

class Accelleratable a where
    accellerate         :: (Velocity -> Velocity) -> a -> a

class Rotatable r where
    twist              :: Tick -> r -> r

-- States
data PlaceState     = PlaceState {place :: Place, velocity :: Velocity}
data RotationState  = RotationState {orient:: Orientation, rotation :: Rotation}

instance Movable PlaceState where
    move (Tick s) (PlaceState place velocity)
                        = PlaceState (vectorMulAdd place velocity s) velocity

instance Accelleratable PlaceState where
    accellerate f state = state {velocity = f (velocity state)}

instance Rotatable RotationState where
    twist (Tick s) (RotationState orient rotation)
                        = RotationState (vectorMulAdd orient rotation s) rotation
