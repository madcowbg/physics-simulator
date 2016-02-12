-----------------------------------------------------------------------------
--
-- Module      :  Swarm.Swarm
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

module Swarm.Swarm (
    Swarm,
    createSwarm,
    evaluate,
    iteration, iterations,
    solution,
    solutionNow
) where

import Swarm.VectorArithm
import Swarm.Types
import Swarm.Settings

-- contains all interesting information about the swarm
data (Num ct, Num vt) => Swarm ct vt = Swarm {particles :: [Particle ct vt]}

--------------------------------------------------------------
-- top-level functions
createSwarm                 :: (Num ct, Num vt) => RndList ct -> ArgSpace ct -> Int -> EvalFunType ct vt -> (Swarm ct vt, RndList ct)
createSwarm rng argspace n fun
                            = let (particles, nextrng) = createRandomList rng (\rng a -> createParticle argspace rng) [1..n]
                                in (evaluate (Swarm particles) fun, nextrng)

-- Methods that update the swarms
updateBest                  :: (Num ct, Num vt, Ord vt) => Swarm ct vt -> Swarm ct vt
updateBest swarm            = updateStatic (updateParticleBest (findBestParticle (particles swarm))) swarm

updatePlace                 :: (Num ct, Num vt, Ord vt) => Swarm ct vt -> Swarm ct vt
updatePlace                 = updateStatic updateParticlePlace

updateSpeed pref            = updateDynamic (updateParticleSpeed pref)

evaluate                    :: (Num ct, Num vt) => Swarm ct vt -> EvalFunType ct vt -> Swarm ct vt
evaluate swarm fun          = Swarm (map (evaluateParticle fun) (particles swarm))

-- Methods to perform iterations on swarms
iteration                   :: (Num ct, Ord ct, Num vt, Ord vt) => Preferences ct -> EvalFunType ct vt -> (Swarm ct vt, RndList ct) -> (Swarm ct vt, RndList ct)
iteration pref func (swarm, rng)  = let
                      step1Swarm            = updateBest swarm
                      (step2Swarm, newrng)  = updateSpeed pref (step1Swarm, rng)
                      step3Swarm            = updatePlace step2Swarm
                      step4Swarm            = evaluate step3Swarm func
                   in (step4Swarm, newrng)

iterations                  :: (Num ct, Ord ct, Num vt, Ord vt) => Preferences ct -> EvalFunType ct vt -> (Swarm ct vt, RndList ct) -> [(Swarm ct vt, RndList ct)]
iterations pref func (swarm, rng) = iterate (iteration pref func) (swarm, rng)

-- Methods to extract results
solution                    :: (Num ct, Num vt, Ord vt) => Swarm ct vt -> Particle ct vt
solution swarm              = findBestParticle (map solutionParticle (particles swarm))

solutionNow                 :: (Num ct, Num vt, Ord vt) => Swarm ct vt -> Particle ct vt
solutionNow swarm           = findBestParticle (particles swarm)

-- Used for calculations that do not need random elements
type StaticFun ct vt        = (Particle ct vt->Particle ct vt)

updateStatic                :: (Num ct, Num vt, Ord vt) => StaticFun ct vt -> Swarm ct vt -> Swarm ct vt
updateStatic func swarm     = Swarm (map func (particles swarm))

-- Used for calculations that require random elements
type DynamicFun ct vt       = RndList ct -> Particle ct vt -> (Particle ct vt, RndList ct)

updateDynamic               :: (Num ct, Num vt) => DynamicFun ct vt -> (Swarm ct vt, RndList ct) -> (Swarm ct vt, RndList ct)
updateDynamic func (swarm, rng)
                            = let (newparts, newrng) = createRandomList rng func (particles swarm)
                               in (Swarm newparts, newrng)
