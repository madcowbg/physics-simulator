-----------------------------------------------------------------------------
--
-- Module      :  Physics.Control.Criteria
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

module Physics.Control.Criteria (
    CriterionW (CriterionW),
    Criterion (Criterion),
    findBestControls,
    ControlStrategy (ControlStrategy,  NoStrategy), controlSequence,
    ControlState (ControlState), steerCraft,
) where

import Physics.Coordinates
import Physics.Elementary
import Physics.Craft.Basic
import Physics.Control.Basic
import Physics.Time

-- TODO Move
import Physics.World

import Swarm.Settings
import Swarm.Utils

-- solver
import Calypso.Core
import Calypso.Instance.PsoVect
import Calypso.Instance.Grade
import System.Random
import Data.Random.Normal

data ControlStrategy = ControlStrategy {controlSequence :: [(Double, ControlState)] } | NoStrategy
data ControlState = ControlState { thrustersState :: [Control]}

steerCraft                  :: (ControlledCraft cc) => ControlState -> cc -> cc
steerCraft state craft      = applyControls craft (thrustersState state)

-- | compares the place of the craft with the target place
comparePlace            :: Craft c => c -> Place -> Double
comparePlace craft place= distance (localPlace (craftCoordinates craft) place)

-- | compares the velocity of the craft with the target velocity
compareVelocity         :: Craft c => c -> Velocity -> Double
compareVelocity craft velocity
                        = distance localVelocity
                          where localVelocity = snd (localState (craftCoordinates craft) (origin, velocity))

-- TODO compareOrientation

data CriterionW         = CriterionW {placeW, velocityW :: Double}
data Criterion          = Criterion {targetPlace :: Place, targetVelocity :: Velocity, weights :: CriterionW}

controlFitness          :: (World w, Craft c) => Criterion -> ([Double] -> w -> w) -> (w -> c) -> w -> [Double] -> Double
controlFitness criterion evolveFunction craftFun world controls
                        = let newcraft = craftFun (evolveFunction controls world)
                            in comparePlace newcraft (targetPlace criterion) * placeW (weights criterion)
                              + compareVelocity newcraft (targetVelocity criterion) * velocityW (weights criterion)

simulateWorld       :: World w => Tick -> Double -> w -> w
simulateWorld tick@(Tick t) time world
                        | time <= 0         = world
                        | otherwise         = simulateWorld tick (time - t) (updateWorld t world)

arrayToSingleStepStrategy   :: [Double] -> ControlStrategy
arrayToSingleStepStrategy vals
                        = ControlStrategy controlSequence
                          where controlSequence = [(1000000, ControlState thrustersState)]
                                thrustersState = map Control vals

attachControls          :: [Double] -> SmallWorld -> SmallWorld
attachControls controls world = world {craftControl = arrayToSingleStepStrategy controls}

attachControlsAndSimulate :: Double -> [Double] -> SmallWorld -> SmallWorld
attachControlsAndSimulate time controls
                        = simulateWorld (Tick 0.025) time . attachControls controls

findBestControls        :: StdGen -> Double -> Criterion -> SmallWorld -> ControlStrategy
findBestControls stdGen time criterion world
                    = arrayToSingleStepStrategy (toList solution)
                      where craftFun    = head . crafts
                            ndim        = length (currentControls (craftFun world))
                            bnds        = (fromList (replicate ndim 0.0), fromList (replicate ndim 1.0))
                            optimFunc   = controlFitness criterion (attachControlsAndSimulate time) craftFun world
                            func        = filterOutsideRange optimFunc . toList
                            guide       = easyOptimize func bnds 10 stdGen
                            solution    = pt guide

filterOutsideRange      :: ([Double] -> Double) -> [Double] -> Double
filterOutsideRange fun v| any (< 0) v       = 100000000000
                        | any (> 1) v       = 100000000000
                        | otherwise         = fun v

