    -----------------------------------------------------------------------------
--
-- Module      :  Physics.AbstractForces
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

module Physics.AbstractForces (
    Force, action,
    ForceAction (ForceAction), actionAmt,
    ShockForce, shock,
    ShockAction (ShockAction, NoShockAction),
    ForceChain (ForceChain, ForceEnd), actOnChain,
    ShockableObj, objPlace,
    PhysicalObj, objMass,
) where

import Physics.Coordinates.FrameOfReference
import Physics.Elementary
import Physics.Time

class ShockableObj o where
    objPlace        :: o -> Place

class (ShockableObj o) => PhysicalObj o where
    objMass             :: o -> Double

-- Forces
class Force f where
    action             :: (PhysicalObj o) => f -> Tick -> StateTriplet -> o -> ForceAction

class ShockForce f where
    shock           :: (ShockableObj o) => f -> Tick -> StateTriplet -> o -> ShockAction

data ForceAction    = ForceAction {actionPlace :: Place, actionAmt :: ForceAmount}
data ShockAction    = ShockAction Place (Velocity->Velocity) | NoShockAction

data ForceChain = forall f. (Force f) => ForceChain {this :: f, next :: ForceChain } | ForceEnd

actOnChain                      :: (PhysicalObj o) => ForceChain -> Tick -> StateTriplet -> o -> [ForceAction]
actOnChain ForceEnd _ _ _       = []
actOnChain (ForceChain this next) tick state obj
                                = action this tick state obj:actOnChain next tick state obj
