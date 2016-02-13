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
    ForceChain (ForceChain, ForceEnd), actOnChain
) where

import Physics.Primitives
import Physics.AbstractObjects
import Physics.Time

-- Forces
class Force f where
    act             :: (PhysicalObj o) => f -> Tick -> Place -> Velocity -> o -> ForceAction

class ShockForce f where
    shock           :: (ShockableObj o) => f -> Tick -> Place -> o -> ShockAction

data ForceAction    = ForceAction {actionPlace :: Place, actionAmt :: ForceAmt}
data ShockAction    = ShockAction Place (Velocity->Velocity) | NoShockAction

data ForceChain = forall f. (Force f) => ForceChain {this :: f, next :: ForceChain } | ForceEnd

actOnChain                      :: (PhysicalObj o) => ForceChain -> Tick -> Place -> Velocity -> o -> [ForceAction]
actOnChain ForceEnd _ _ _ _      = []
actOnChain (ForceChain this next) tick place vel obj
                                = act this tick place vel obj:actOnChain next tick place vel obj
