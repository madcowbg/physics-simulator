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
    SmallWorld (SmallWorld),
    World, updateWorld, runTime, runForces
) where

import Physics.Forces
import Physics.AbstractObjects
import Physics.Objects
import Physics.Craft.Rigid
import Physics.AbstractForces
import Physics.Time

--data GlobalForces = GlobalForces {gravity :: Gravity, ground :: GroundForce}
data SmallWorld = SmallWorld {crafts :: [RigidCraft], ground :: BouncingGround} --, forces :: GlobalForces}
--data GlobalForce = forall f. Force f => GlobalForce {force :: f} -- another way to incorporate forces -> split action and view

class World w where
    updateWorld          :: Double -> w -> w
    updateWorld time     = let t = Tick time in runTime t . runForces t
    runTime         :: Tick -> w -> w
    runForces           :: Tick -> w -> w

instance World SmallWorld where
    runTime t world
                    = world {crafts = map (move t) (crafts world)}
    runForces t world   = world {crafts = map (executeForces t) (crafts world)}

