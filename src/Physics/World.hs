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
module Physics.World (
    SmallWorld (SmallWorld),
    World, updateWorld, runTime, runForces
) where

import Physics.Forces
import Physics.Objects
import Physics.Craft.Rigid
import Physics.AbstractForces
import Physics.Time

data SmallWorld = SmallWorld {crafts :: [RigidCraft], ground :: BouncingGround}

class World w where
    updateWorld          :: Double -> w -> w
    updateWorld time     = let t = Tick time in runTime t . runForces t
    runTime         :: Tick -> w -> w
    runForces           :: Tick -> w -> w

instance World SmallWorld where
    runTime t world
                    = world {crafts = map (twist t . move t) (crafts world)}
    runForces t world   = world {crafts = map (executeForces t) (crafts world)}

