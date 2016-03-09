-----------------------------------------------------------------------------
--
-- Module      :  Physics.World.Basic
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

module Physics.World.Basic (
    World, updateWorld, runTime, runForces
) where
import Physics.Time

class World w where
    updateWorld         :: Double -> w -> w
    updateWorld time    = let t = Tick time in runTime t . runForces t
    runTime         :: Tick -> w -> w
    runForces           :: Tick -> w -> w

