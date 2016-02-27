-----------------------------------------------------------------------------
--
-- Module      :  Swarm.Types
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

module Swarm.Types (
    Particle,
    RndList(RndList),
    createRandomList,
    updateParticlePlace,
    createParticle,
    EvalFunType,
    evaluateParticle,
    findBestParticle,
    updateParticleBest,
    updateParticleSpeed,
    solutionParticle,
    showParticle,
    particleCoords
) where

import Data.List
import Data.Function

import qualified Data.Vector as V
import Swarm.VectorArithm
import Swarm.Settings

-- contains all interesting information about each particle
data (Num ct, Num vt) => Particle ct vt = Particle {
    coords :: ParticleState ct,
    currentVal :: vt,
    localBest :: Particle ct vt,
    globalBest :: Particle ct vt}
    | InitialParticle {coords :: ParticleState ct}
    | EvaluatedInitialParticle {coords :: ParticleState ct, currentVal :: vt}

data (Num ct) => ParticleState ct =  ParticleState {pos, vel :: Vector ct}


class BoundCoerce a where
applyBound                      :: (Num a) => CoordBound a -> a -> a

instance (Num a) => BoundCoerce (CoordBound a) where
applyBound (CoordBound origin scale) val               = origin + val*scale

-- evaluate
type EvalFunType ct vt = Vector ct -> vt
evaluateParticle                                    :: (Num ct, Num vt) => EvalFunType ct vt -> Particle ct vt -> Particle ct vt
evaluateParticle f (InitialParticle crds)           = EvaluatedInitialParticle crds (f (pos crds))
evaluateParticle f (EvaluatedInitialParticle crds _)= EvaluatedInitialParticle crds (f (pos crds))
evaluateParticle f (Particle crds val lbest gbest)  = Particle crds (f (pos crds)) lbest gbest

type RandFun g b a              = g -> b -> (a, g)
createRandomList                :: (Num ct) => RndList ct -> RandFun (RndList ct) b a -> [b] -> ([a], RndList ct)
createRandomList rng _ []       = ([], rng)
createRandomList rng f (b:bs)   = let thisrnd = f rng b  -- exec first
                                      nextrnds = createRandomList (snd thisrnd) f bs -- exect next
                                    in (fst thisrnd:fst nextrnds, snd nextrnds)

pop                             :: (Num a) => RndList a -> (a, RndList a)
pop (RndList oldlst oldtaken)   = (head oldlst, RndList (tail oldlst) (oldtaken + 1))

createParticle                  :: (Num ct) => ArgSpace ct -> RndList ct -> (Particle ct vt, RndList ct)
createParticle (ArgSpace bounds) rng
                                = let (coords, rng1) = createRandomList rng randCoord bounds
                                      (vels, rng2)  = createRandomList rng1 randCoord bounds
                                  in (InitialParticle (ParticleState (V.fromList coords) (V.fromList vels)), rng2)

randCoord                       :: (Num ct) => RndList ct -> CoordBound ct -> (ct, RndList ct)
randCoord rng coordbnd          = let (val, nextrng) = pop rng
                                    in (applyBound coordbnd val, nextrng)


updateParticlePlace         :: (Num ct, Num vt, Ord vt) => Particle ct vt -> Particle ct vt
updateParticlePlace (Particle crds eval lbest gbest)
                            = Particle (ParticleState (updPos crds) (vel crds)) eval lbest gbest


updPos crds             = add (pos crds) (vel crds)

updSpeed                :: (Num ct, Ord ct) => Preferences ct -> ParticleState ct -> Vector ct -> Vector ct -> ParticleState ct -> ParticleState ct -> Vector ct
updSpeed (Preferences inertia cp cg vMax) state accelL accelG lbest gbest
                        =  cap vMax
                            (add (scl inertia (vel state))
                               (add (scl cp (mul accelL (sub (pos lbest) (pos state))))
                                    (scl cg (mul accelG (sub (pos gbest) (pos state))))))


stripParticle :: (Num ct, Num vt, Ord vt) => Particle ct vt -> Particle ct vt
stripParticle (InitialParticle _) = error "cannot strip initial particles"
stripParticle part@ (EvaluatedInitialParticle _ _)
                    = part
stripParticle (Particle crds eval lbest gbest)
                    = EvaluatedInitialParticle crds eval

findBestParticle                :: (Num ct, Num vt, Ord vt) => [Particle ct vt] -> Particle ct vt
findBestParticle part           = stripParticle (minimumBy (compare `on` currentVal) part)

updateParticleBest      :: (Num ct, Num vt, Ord vt) => Particle ct vt -> Particle ct vt -> Particle ct vt
updateParticleBest ngbest part@(EvaluatedInitialParticle crds eval)
                    = Particle crds eval (stripParticle part) ngbest
updateParticleBest ngbest part@(Particle crds eval lbest gbest)
                    = Particle crds eval newlbest newgbest
                    where newlbest = if eval < currentVal lbest
                                     then stripParticle part
                                     else lbest
                          newgbest = if currentVal ngbest < currentVal gbest
                                     then ngbest
                                     else gbest

solutionParticle    :: (Num ct, Num vt, Ord vt) => Particle ct vt -> Particle ct vt
solutionParticle (InitialParticle _)
                    = error "cannot find current solution for INITIAL particles"
solutionParticle part@(EvaluatedInitialParticle _ _)
                    = part
solutionParticle (Particle _ _ _ gbest)
                    = gbest


updateParticleSpeed     :: (Num ct, Ord ct, Num vt) => Preferences ct -> RndList ct -> Particle ct vt -> (Particle ct vt, RndList ct)
updateParticleSpeed pref rng (Particle crds eval lbest gbest)
                        = let (accelL, newrng1) = createRandomList rng (\rng l -> pop rng) (V.toList (vel crds))
                              (accelG, newrng2) = createRandomList newrng1 (\rng l -> pop rng) (V.toList (vel crds))
                              in (Particle (ParticleState (pos crds) (updSpeed pref crds (V.fromList accelL) (V.fromList accelG) (coords lbest) (coords gbest))) eval lbest gbest, newrng2)


showParticle            :: Particle Float Float -> String
showParticle part       = "" ++ show (currentVal part) ++ ", pos=" ++ show (pos (coords part)) ++ ", vel=" ++ show (vel (coords part)) ++ "."

particleCoords          :: Particle Double Double -> [Double]
particleCoords particle = V.toList (pos (coords particle))
