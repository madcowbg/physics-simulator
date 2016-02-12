-----------------------------------------------------------------------------
--
-- Module      :  Physics.Objects
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
{-# LANGUAGE ExistentialQuantification #-}

module Physics.Objects (
    PlaceState (PlaceState),
    RotationState (RotationState),
    PhysicalObj (PhysicalObj),
    Craft (Craft),
    craftMass
) where

import Physics.Primitives

data Craft          = Craft {parts :: [PhysicalObj], placeState :: PlaceState, rotationState :: RotationState}

craftMass           :: Craft -> Double
craftMass (Craft parts _ _)
                    = sum (map mass parts)

-- Particle State
data PhysicalObj    = PhysicalObj {localPlace :: Place, mass :: Double}

-- States
data PlaceState     = PlaceState {place :: Place, velocity :: Velocity}
data RotationState  = RotationState {orient:: Orientation, rotation :: Rotation, angularMomentum :: Rotation -> Double}

