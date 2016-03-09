-----------------------------------------------------------------------------
--
-- Module      :  Physics.Planning.Linear
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

module Physics.Planning.Linear (

) where

import Physics.Elementary
import Physics.Coordinates
import Physics.Primitives

type Direction          = Vector3

data Trajectory         = LinearTrajectory {beg :: Place, end :: Place, fun :: Double -> Place}

linearPlanner           :: Place -> Place -> Double -> Trajectory
linearPlanner from to max
                        = LinearTrajectory from to linearInterpolate

linearInterpolate       :: Place -> Place -> Double -> Double -> Place
linearInterpolate from to maxVel t
                        = from + vectorWithLength (to-from) (max (maxVel*t) (vectorLength (to-from)))

