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
import Physics.Craft.Rocket
import Physics.AbstractForces
import Physics.Time

data SmallWorld = SmallWorld {currentTime :: Double, crafts :: [Rocket], ground :: BouncingGround, control :: ControlStrategy}

class World w where
    updateWorld         :: Double -> w -> w
    updateWorld time    = let t = Tick time in runTime t . runForces t
    runTime         :: Tick -> w -> w
    runForces           :: Tick -> w -> w

instance World SmallWorld where
    runTime t@(Tick s) world
                        = world {currentTime = currentTime world + s, crafts = map (twist t . move t) (crafts world)}
    runForces t world   = world {crafts = map (executeForces t) (crafts world)}
    updateWorld time    = let t = Tick time in runTime t . runForces t . executeStrategy t


executeStrategy         :: Tick -> SmallWorld -> SmallWorld
executeStrategy tick@(Tick t) world@(SmallWorld _ _ _ NoStrategy)
                        = world
executeStrategy tick@(Tick t) world
                        | null (controlSequence (control world))
                                                    = world
                        | fst (head (controlSequence (control world))) <= currentTime world
                                                   = executeStrategy tick (world { control = skipFirstControl (control world)})
                        | otherwise                = world {crafts = [steerCraft (snd $ head $ controlSequence $ control world) (head $ crafts world)]}

skipFirstControl        :: ControlStrategy -> ControlStrategy
skipFirstControl control= control {controlSequence = tail (controlSequence control)}

