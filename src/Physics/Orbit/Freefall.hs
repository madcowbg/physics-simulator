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
) where

import qualified Linear.Quaternion as Q -- TODO move to primitives
import Linear.Matrix -- TODO move to primitives
import Linear.Metric -- TODO move to primitives
import Linear.Vector -- TODO move to primitives
import Linear.V3     -- TODO move to primitives
import Control.Lens

import Physics.Elementary

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
                            __M     = (meanFromEllipticalAnomaly __e . ellipticalFromTrueAnomaly __e) _nu

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


_mubody body    = gravityConst * mass body

-- algorithm from https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf
newton epsilon f f' guess = let newGuess = guess - (f guess / f' guess)
                                err =  abs (newGuess - guess)
                            in if err < epsilon
                                  then newGuess
                                  else newton epsilon f f' newGuess

fromOrbitToState    :: CelestialBody -> Orbit -> Time -> IState
fromOrbitToState body orbit@(Orbit orbitalParams _nu _M_0) deltaT
                    = rotateToBodyInertial orbitalParams (IState r_o v_o) -- transform to inertial frame
                      where _mu     = _mubody body
                            _M_t    = normalizeAngle (_M_0 + deltaT * sqrt (_mu / (_a orbitalParams ** 3)))
                            f_E _E  = _E - _e orbitalParams * sin _E - _M_t
                            f_E' _E = 1 - _e orbitalParams * cos _E
                            -- numerical solve for eccentric anomaly
                            _E = newton 1e-4 f_E f_E' _M_t
                            -- true anomaly
                            _nu = 2 * atan2 (sqrt (1+_e orbitalParams) * sin (_E/2)) (sqrt (1-_e orbitalParams) * cos (_E/2))
                            -- distance to central body
                            _rc = _a orbitalParams * (1 - _e orbitalParams * cos _E)
                            -- in orbital coordinates
                            r_o = _rc *^ makevect (cos _nu) (sin _nu) 0
                            v_o = (sqrt (_mu * _a orbitalParams) / _rc) *^ makevect (- sin _E) (sqrt (1 - _e orbitalParams ** 2) * cos _E) 0


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





