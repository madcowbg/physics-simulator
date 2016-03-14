-----------------------------------------------------------------------------
--
-- Module      :  Physics.World.Small
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

module Physics.World.Small (
    SmallWorld (SmallWorld), crafts, craftControl,
    findBestControls,
) where
import Physics.Forces
import Physics.Craft.Basic
import Physics.Craft.Rocket
import Physics.AbstractForces
import Physics.Time
import Physics.Control.Criteria

-- solver
import Calypso.Core
import Calypso.Instance.PsoVect
import Calypso.Instance.Grade
import System.Random
import Data.Random.Normal

import Physics.World.Basic
import Physics.Control.Basic

data SmallWorld = SmallWorld {currentTime :: Double, crafts :: [Rocket], ground :: BouncingGround, gravity :: Gravity, craftControl :: ControlStrategy}

instance World SmallWorld where
    runTime t@(Tick s) world
                        = world {currentTime = currentTime world + s, crafts = map (changeOrientation t . changePosition t) (crafts world)}
    runForces t world   = world {crafts = map (executeForces t) (crafts world)}
    updateWorld time    = let t = Tick time in runTime t . runForces t . executeStrategy t


executeStrategy         :: Tick -> SmallWorld -> SmallWorld
executeStrategy tick@(Tick t) world@(SmallWorld {craftControl = NoStrategy})
                        = world
executeStrategy tick@(Tick t) world
                        | null (controlSequence (craftControl world))
                                                    = world
                        | fst (head (controlSequence (craftControl world))) <= currentTime world
                                                   = executeStrategy tick (world { craftControl = skipFirstControl (craftControl world)})
                        | otherwise                = world {crafts = [steerCraft (snd $ head $ controlSequence $ craftControl world) (head $ crafts world)]}

skipFirstControl        :: ControlStrategy -> ControlStrategy
skipFirstControl craftControl= craftControl {controlSequence = tail (controlSequence craftControl)}

attachControls          :: [Double] -> SmallWorld -> SmallWorld
attachControls controls world = world {craftControl = arrayToSingleStepStrategy controls}

attachControlsAndSimulate :: Double -> [Double] -> SmallWorld -> SmallWorld
attachControlsAndSimulate time controls
                        = simulateWorld (Tick 0.025) time . attachControls controls

findBestControls        :: Double -> Criterion -> SmallWorld -> ControlStrategy
findBestControls time criterion world
                    = arrayToSingleStepStrategy (toList solution)
                      where craftFun    = head . crafts
                            ndim        = length (currentControls (craftFun world))
                            bnds        = (fromList (replicate ndim 0.0), fromList (replicate ndim 1.0))
                            optimFunc   = controlFitness criterion (attachControlsAndSimulate time) craftFun world
                            func        = filterOutsideRange optimFunc . toList
                            guide       = easyOptimize func bnds 5 (mkStdGen 5)
                            solution    = pt guide

filterOutsideRange      :: ([Double] -> Double) -> [Double] -> Double
filterOutsideRange fun v| any (< 0) v       = 100000000000
                        | any (> 1) v       = 100000000000
                        | otherwise         = fun v
