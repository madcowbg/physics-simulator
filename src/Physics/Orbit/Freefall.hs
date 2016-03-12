-----------------------------------------------------------------------------
--
-- Module      :  Physics.Orbit.Freefall
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

module Physics.Orbit.Freefall (
    Orbit(Orbit),
    OrbitalParams(OrbitalParams),
    IState(IState), position,
    CelestialBody(CelestialBody),
    fromStateToOrbit,
    fromOrbitToState,
    -- ?
    trueAnomaly, meanAnomaly,
    calculateSigleStepNeededVelocity,
) where

import qualified Linear.Quaternion as Q -- TODO move to primitives
import Linear.Matrix -- TODO move to primitives
import Linear.Metric -- TODO move to primitives
import Linear.Vector -- TODO move to primitives
import Linear.V3     -- TODO move to primitives
import Control.Lens
import Numeric.AD
import Numeric.AD.Newton.Double

import qualified Calypso.Core as C
import Calypso.Instance.PsoVect
import Calypso.Instance.Grade
import System.Random

import Physics.Elementary

import Debug.Trace

type Angle      = Double
type Distance   = Double
type Time       = Double
type Ratio      = Double
type Energy     = Double

data OrbitalParams = OrbitalParams {_a      :: Distance, -- semiMajorAxis
                    _e      :: Ratio, -- eccentricity
                    _i      :: Angle, -- inclination
                    _omega  :: Angle, -- argument of periapsis
                    _Omega  :: Angle} -- longitude of ascending node

data Orbit = Orbit {params :: OrbitalParams,
                    _nu     :: Angle, -- true anomaly
                    _M      :: Angle}   --mean anomaly

trueAnomaly     = _nu
meanAnomaly     = _M

gravityConst   = 1 -- NOT 9.80665 -- m/s^2


data IState = IState {position :: Place, -- place in ECI
                      velocity :: Velocity} -- velocity in ECI

data CelestialBody = CelestialBody {mass :: Double} --WTF?

-- algorithm from http://space.stackexchange.com/questions/1904/how-to-programmatically-calculate-orbital-elements-using-position-velocity-vecto
-- alt: https://downloads.rene-schwarz.com/download/M002-Cartesian_State_Vectors_to_Keplerian_Orbit_Elements.pdf

-- polar vector
_Khat              = makevect 0 0 1

fromStateToOrbitalParams    :: CelestialBody -> IState -> OrbitalParams
fromStateToOrbitalParams body state = params (fromStateToOrbit body state)

fromStateToOrbit :: CelestialBody -> IState -> Orbit
fromStateToOrbit body state@(IState r v)
                    = Orbit {
                            params = OrbitalParams {
                                _a     = - _mu / (2 * _E),                         -- semi-major axis
                                _e     = __e,                                      -- eccentricity
                                _i     = acos (_harr ^. _z / norm _harr),          -- inclination
                                _omega = calcArgumentOfPeriapsis _earr _z _nhat,   -- argument of periapsis
                                _Omega = acos (_nhat ^._x / _n)},                  -- longitude of ascending node
                            _nu     = _nu,                                      -- true anomaly
                            _M      = __M}                                      -- mean anomaly
                      where
                            _r      = norm r
                            -- standard gravitational parameter
                            _mu     = _mubody body
                            -- angular momentum
                            _harr   = cross r v
                            -- node vector
                            _nhat   = cross _Khat _harr
                            _n      = norm _nhat
                            -- excentricity vector
                            _earr   = cross v _harr ^/ _mu - (r ^/ _r)
                            __e      = norm _earr
                            -- specific mechanical energy
                            _E      = quadrance v / 2 - _mu / _r
                            -- true anomaly
                            _nu     = calcTrueAnomaly _earr state
                            -- mean anomaly
                            __M     = trueToMeanAnomaly __e _nu

angularMomentum     :: IState -> Vector3
angularMomentum (IState r v) = cross r v

calcArgumentOfPeriapsis _earr _z _nhat
                    | _earr ^. _z < 0   = 2 * pi - _omega
                    | otherwise         = _omega
                    where _omega  = acos (dot _nhat _earr / (norm _nhat * norm _earr))

calcTrueAnomaly     :: Vector3 -> IState -> Double
calcTrueAnomaly _earr (IState r v)
                    | dot r v < 0  = 2 * pi - _nu
                    | otherwise    = _nu
                    where _nu = acos (dot _earr r / (norm _earr * norm r))

ellipticalFromTrueAnomaly
                    :: Double -> Double -> Double
ellipticalFromTrueAnomaly __e _nu
                    = 2 * atan (tan (_nu / 2) / sqrt ((1 + __e)/(1-__e)))

meanFromEllipticalAnomaly       :: Double -> Double -> Double
meanFromEllipticalAnomaly __e __E = __E - __e * sin __E

trueToMeanAnomaly __e =  (meanFromEllipticalAnomaly __e . ellipticalFromTrueAnomaly __e)

_mubody body    = gravityConst * mass body

-- algorithm from https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf
-- FIXME use findZero
newton epsilon iter f f' guess
            | iter < 0          = guess
            | err < epsilon     = newGuess
            | otherwise         = newton epsilon (iter - 1) f f' newGuess
            where newGuess = guess - (f guess / f' guess)
                  err =  abs (newGuess - guess)

--newtonAD                :: Double -> (Double -> Double) -> Double -> Double
--newtonAD epsilon f guess = newton epsilon f (diff f) guess

fromOrbitToState    :: CelestialBody -> Orbit -> Time -> IState
fromOrbitToState body orbit@(Orbit orbitalParams _nu _M_0) deltaT
                    = fromOrbitAndTrueAnomalyToState body orbit _nu
                      where _mu     = _mubody body
                            _M_t    = normalizeAngle (_M_0 + deltaT * sqrt (_mu / (_a orbitalParams ** 3)))
                            _E = meanToEllipticalAnomaly orbitalParams _M_t
                            -- true anomaly
                            _nu = ellipticalToTrueAnomaly orbitalParams _E

fromOrbitAndTrueAnomalyToState  :: CelestialBody -> Orbit -> Time -> IState
fromOrbitAndTrueAnomalyToState body orbit@(Orbit orbitalParams _nu_0 _M_0) _nu
                    = rotateToBodyInertial orbitalParams (IState r_o v_o) -- transform to inertial frame
                      where
                            _mu     = _mubody body
                            _E = ellipticalFromTrueAnomaly (_e orbitalParams) _nu
                            -- distance to central body
                            _rc = _a orbitalParams * (1 - _e orbitalParams * cos _E)
                            -- in orbital coordinates
                            r_o = _rc *^ makevect (cos _nu) (sin _nu) 0
                            v_o = (sqrt (_mu * _a orbitalParams) / _rc) *^ makevect (- sin _E) (sqrt (1 - _e orbitalParams ** 2) * cos _E) 0

maxIter = 100

meanToEllipticalAnomaly :: OrbitalParams -> Angle -> Angle
meanToEllipticalAnomaly orbitalParams _M_t = newton 1e-4 maxIter f_E f_E' _M_t                -- numerical solve for eccentric anomaly
                                where
                                    f_E _E  = _E - _e orbitalParams * sin _E - _M_t
                                    f_E' _E = 1 - _e orbitalParams * cos _E

ellipticalToTrueAnomaly     :: OrbitalParams -> Angle -> Angle
ellipticalToTrueAnomaly orbitalParams _E
            = 2 * atan2 (sqrt (1+_e orbitalParams) * sin (_E/2)) (sqrt (1-_e orbitalParams) * cos (_E/2))

meanToTrueAnomaly       :: OrbitalParams -> Angle -> Angle
meanToTrueAnomaly orbitalParams = ellipticalToTrueAnomaly orbitalParams . meanToEllipticalAnomaly orbitalParams

rotateToBodyInertial    :: OrbitalParams -> IState -> IState
rotateToBodyInertial orbitalParams (IState r_o v_o)
                        = IState {position = Q.rotate fullTransform r_o, velocity = Q.rotate fullTransform v_o}
                          where _R_Omega = Q.axisAngle (makevect 0.0 0 1) ( _Omega orbitalParams)
                                _R_i    = Q.axisAngle (makevect 1.0 0 0) ( _i orbitalParams)
                                _R_omega = Q.axisAngle (makevect 0.0 0 1) ( _omega orbitalParams)
                                fullTransform = _R_Omega * _R_i * _R_omega

--atan2       :: Double -> Double -> Double
--atan2 0 y   | y > 0     = pi / 2
--            | y < 0     = -pi / 2
--            | otherwise = error
--atan2 x y   | x > 0     = atan (y/x)
--            | y >= 0 && x < 0   = atan (y/x) + pi
--            | y < 0  && x < 0   = atan (y/x) - pi
--            | otherwise = error

-- | Normalize an angle to be between 0 and 2*pi radians
normalizeAngle :: Double -> Double
normalizeAngle f = f - 2 * pi * floor' (f / (2 * pi))
 where  floor' :: Double -> Double
        floor' x = fromIntegral (floor x :: Int)
{-# INLINE normalizeAngle #-}


-- now this gets interesting
type DeltaV = Vector3

--data TwoStepTrajectory = TwoStepTrajectory {interceptTime :: Double, alpha :: Angle, deltaVIntercept :: DeltaV, deltaVFinal :: DeltaV}
--
--totalDeltaV         :: TwoStepTrajectory -> Double
--totalDeltaV traj    = norm (deltaVIntercept traj) + norm (deltaVFinal traj)
--
--epsilonT = 1e-3
--
--calculateDirectIntercept    :: CelestialBody -> IState -> Place -> TwoStepTrajectory    -- add facility to reject trajectories & vel target
--calculateDirectIntercept body state@(IState p v) targetPlace
--        = let
--            _OX = targetPlace
--            presentOrbit = fromStateToOrbit body state
--            deltaT = extremum (totalDeltaV . optimalForT presentOrbit) 0
--          in optimalForT presentOrbit deltaT
--          where
--                optimalForT :: Orbit -> Double -> TwoStepTrajectory
--                optimalForT presentOrbit dT
--                        = let
--                            stateBeforeTrajChange = fromOrbitToState body presentOrbit dT
--                            _OA = position stateBeforeTrajChange
--                            _AO = -_OA
--                            _h = cross _OA _OX  -- normal of intercept trajectory (specific angular momentum)
--                            alpha = extremum (totalDeltaV . optimalForAlpha stateBeforeTrajChange) 0
--                          in optimalForAlpha stateBeforeTrajChange
--                          where
--                            optimalForAlpha     :: IState -> TwoStepTrajectory
--                            optimalForAlpha stateBeforeTrajChange
--                                    = let
--                                        deltaVSize = extremum (\size -> norm (target - _closestX size)) 0
--                                        _vunit = rotate (axisAngle _h alpha) (unitVector _AO)
--                                        _v = deltaVSize *^ _vunit   -- proper size of velocity at intercept trajectory start
--                                        interceptOrbit = fromStateToOrbit body (IState _OA _v)
--
--                                        trueToMeanAnomaly __e _nu


--                            _h = cross _OA _OX  -- normal of intercept trajectory (specific angular momentum)
--
calculateSigleStepNeededVelocity    :: CelestialBody -> IState -> Place -> Angle -> IState
calculateSigleStepNeededVelocity body state@(IState p v) targetPlace alpha
            = let
                _OX = targetPlace
                _OA = p
                _h = cross _OX _OA  -- needed normal of intercept trajectory (specific angular momentum)
                _hunit = _h ^/ norm _h
                deltaTrueAnomaly = acos (dot _OA _OX / (norm _OA * norm _OX))
                _AOunit = _OA ^* (-1 / norm _OA)
                vunit = Q.rotate (Q.axisAngle _hunit alpha) _AOunit
                optV = univariateMin (-600) 600 (\vmag -> norm (_OX - _OZ _OA deltaTrueAnomaly vunit vmag))
              in traceShow (optV) $ IState _OA (optV *^ vunit)
              where
                _OZ     :: Place -> Double -> Vector3 -> Double -> Vector3    -- place after moving as close to target as possible
                _OZ _OA deltaTrueAnomaly vunit vsize
                            = let
                                _AB = vsize *^ vunit
                                interceptOrbit = fromStateToOrbit body (IState _OA _AB)
                                trueAnomalyOfTarget = deltaTrueAnomaly - _nu interceptOrbit
                                --_nuinteceptOrbit
                                closestState = fromOrbitAndTrueAnomalyToState body interceptOrbit trueAnomalyOfTarget
                              in position closestState

univariateMin      :: Double -> Double -> (Double -> Double) -> Double
univariateMin min max f = let guide = C.easyOptimize f (min, max) 100 (mkStdGen 0) in C.pt guide
--                            where bnds = C.PsoGuide min max
