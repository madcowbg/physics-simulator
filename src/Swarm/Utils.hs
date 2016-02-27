-----------------------------------------------------------------------------
--
-- Module      :  Swarm.Utils
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

module Swarm.Utils (
    sumsq,
--    example,
    createBasicSwarm,
    executeSwarm
) where
import Swarm.VectorArithm
import Swarm.Settings

------- examples
import System.Random
import Data.Random.Normal

import Swarm.Swarm
import Swarm.Types

import qualified Data.Vector as V
-----------------------------------------

decardSpace             :: (Num a) => CoordBound a -> Int -> ArgSpace a
decardSpace bnd dim     = ArgSpace (replicate dim bnd)


sumsq                   :: Vector Float -> Float
sumsq v                 = sum (add (mul v v) v)

createBasicSwarm        :: CoordBound Double -> Int -> Int -> ([Double] -> Double) -> (Swarm Double Double, RndList Double)
createBasicSwarm bnd ndim steps func
                        = createSwarm (RndList floatList 0) (ArgSpace (replicate ndim bnd)) ndim (func . V.toList)


defaultPreferences      = Preferences 0.3 1.4 1.4 2
executeSwarm            :: Int -> (Swarm Double Double, RndList Double) -> ([Double] -> Double) -> [Double]
executeSwarm steps statetuple func
                        = particleCoords (solution (fst (iterations defaultPreferences (func . V.toList) statetuple !! steps)))

------- examples

floatList       :: [Double]
floatList       = normals myRandomGen
                    where myRandomGen = mkStdGen 1

--example :: IO ()
--example = do
--        let samples    = floatList
--        let bnd        = CoordBound 0 1
--        let preferences = Preferences 0.3 1.4 1.4 2
--        let func = sumsq
--        let (swarm, rng) = createSwarm (RndList samples 0) (ArgSpace (replicate 2 bnd)) 50 func
--        let swarm2 = evaluate swarm func
--        let iters = iterations preferences func (swarm2, rng)
--        let (swarm3, rng2) = iters !! 1000
--        let best = solution swarm3
--        --print (show (taken rng))
--        --print (show (length (particles swarm3)))
--        print 5
--        print (showParticle best)
--        print (showParticle (solutionNow swarm2))

