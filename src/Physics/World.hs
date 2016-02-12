-----------------------------------------------------------------------------
--
-- Module      :  Physics.World
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

module Physics.World (
    World (World),
    GlobalForce (GlobalForce),
    force
) where

import Physics.Forces
import Physics.Objects

data World = World {crafts :: [Craft], globalForces :: [GlobalForce], ground :: GroundForce}
data GlobalForce = forall f. Force f => GlobalForce {force :: f} -- another way to incorporate forces -> split action and view

