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
    Trajectory(LinearTrajectory),
 --   linearPlanner, linearInterpolate,
) where

import Physics.Elementary
import Physics.Coordinates.Inertial
import Physics.Primitives

data Trajectory a         = LinearTrajectory {beg :: a, end :: a, fun :: a -> Place}
--
--linearPlanner           :: Place -> Place -> Double -> Trajectory
--linearPlanner from to maxVel
--                        = LinearTrajectory from to (linearInterpolate from to maxVel)

--linearInterpolate       :: Vector3 -> Vector3 -> Double -> Double -> Vector3
--linearInterpolate from to maxVel t
--                        = from + vectorWithLength (to-from) (max (maxVel*t) (vectorLength (to-from)))


--stopMovement            :: CoordinateSystem -> Acceleration
--stopMovement system     = let (place, velocity) = globalState system (origin, atrest)
--                          in
