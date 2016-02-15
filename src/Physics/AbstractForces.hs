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
    Force, act,
    ForceAction (ForceAction), actionAmt,
    ShockForce, shock,
    ShockAction (ShockAction, NoShockAction),
    ForceChain (ForceChain, ForceEnd), actOnChain,
    ShockableObj, objPlace,
    PhysicalObj, objMass,
) where

import Physics.Elementary
import Physics.Time

class ShockableObj o where
    objPlace        :: o -> Place

class (ShockableObj o) => PhysicalObj o where
    objMass             :: o -> Double

-- Forces
class Force f where
    act             :: (PhysicalObj o) => f -> Tick -> Place -> Velocity -> o -> ForceAction

class ShockForce f where
    shock           :: (ShockableObj o) => f -> Tick -> Place -> o -> ShockAction

data ForceAction    = ForceAction {actionPlace :: Place, actionAmt :: ForceAmt}
data ShockAction    = ShockAction Place (Velocity->Velocity) | NoShockAction

data ForceChain = forall f. (Force f) => ForceChain {this :: f, next :: ForceChain } | ForceEnd

actOnChain                      :: (PhysicalObj o) => ForceChain -> Tick -> (Place, Velocity) -> o -> [ForceAction]
actOnChain ForceEnd _ _ _       = []
actOnChain (ForceChain this next) tick globalState obj
                                = uncurry (act this tick) globalState obj:actOnChain next tick globalState obj
