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
    IState(IState),
    CelestialBody(CelestialBody),
    fromStateToOrbit,
    fromOrbitToState,
    -- ?
    _nu, _M, statePlace,
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

data Orbit = Orbit {_a      :: Distance, -- semiMajorAxis
                    _e      :: Ratio, -- eccentricity
                    _i      :: Angle, -- inclination
                    _omega  :: Angle, -- argument of periapsis
                    _Omega  :: Angle, -- longitude of ascending node
--                  _T      :: Time}  -- time of periapsis passage
                    _nu      :: Angle, -- true anomaly
                    _M      :: Angle,   --mean anomaly
                    _p      :: Time}  -- time for full orbit

gravityConst   = 1 -- NOT 9.80665 -- m/s^2


data IState = IState {statePlace :: Place, -- place in ECI
                      v :: Velocity} -- velocity in ECI

data CelestialBody = CelestialBody {mass :: Double} --WTF?

-- algorithm from http://space.stackexchange.com/questions/1904/how-to-programmatically-calculate-orbital-elements-using-position-velocity-vecto
-- alt: https://downloads.rene-schwarz.com/download/M002-Cartesian_State_Vectors_to_Keplerian_Orbit_Elements.pdf

-- polar vector
_Khat              = makevect 0 0 1

fromStateToOrbit    :: CelestialBody -> IState -> Orbit
fromStateToOrbit body (IState r v)
                    = Orbit {_a     = __a,                                               -- semi-major axis
                             _e     = __e,                                               -- eccentricity
                             _i     = acos (_harr ^. _z / norm _harr),                   -- inclination
                             _omega = if _earr ^. _z < 0 then 2 * pi - _omega else _omega, -- argument of periapsis
                             _Omega = acos (_nhat ^._x / _n),                           -- longitude of ascending node
                            _nu     = _nu,                                              -- true anomaly
                            _M      = __M,
                            _p      = __p}                                               -- time for full orbit
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
                            --_earr   = ((quadrance v - _mu / _r) *^ r - dot r v *^ v) ^/ _mu
                            _earr   = cross v _harr ^/ _mu - (r ^/ _r)
                            __e      = norm _earr
                            -- specific mechanical energy
                            _E      = quadrance v / 2 - _mu / _r
                            -- semi-major axis
                            __a      = - _mu / (2 * _E)
                            --TODO WTF is that, period?
                            __p      = __a * (1 - __e * __e)
                            -- argument of periapsis
                            _omega  = acos (dot _nhat _earr / (_n * __e))
                            -- position in orbit
                            _nuT     = acos (dot _earr r / (__e * _r))
                            _nu     = if dot r v < 0 then 2 * pi - _nuT else _nuT
                            __E     = 2 * atan (tan (_nu / 2) / (sqrt ((1 + __e)/(1-__e))))
                            -- mean anomaly
                            __M     = __E - __e * sin __E


_mubody body    = gravityConst * mass body

-- algorithm from https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf
newton epsilon f f' guess = let newGuess = guess - (f guess / f' guess)
                                err =  abs (newGuess - guess)
                            in if err < epsilon
                                  then newGuess
                                  else newton epsilon f f' newGuess

fromOrbitToState    :: CelestialBody -> Orbit -> Angle -> IState
fromOrbitToState body orbit mean_anomaly
                    = IState {statePlace = Q.rotate fullTransform r_o, v = Q.rotate fullTransform v_o}
                      where
                            _mu     = _mubody body
                            _M_t    = mean_anomaly
                            f_E _E  = _E - _e orbit * sin _E - _M_t
                            f_E' _E = 1 - _e orbit * cos _E
                            -- numerical solve for eccentric anomaly
                            --_E = _M_t -- FIXME newton 1e-2 f_E f_E' _M_t
                            _E = newton 1e-4 f_E f_E' _M_t
                            -- true anomaly
                            _nu = 2 * atan2 (sqrt (1+_e orbit) * sin (_E/2)) (sqrt (1-_e orbit) * cos (_E/2))
                            -- distance to central body
                            _rc = _a orbit * (1 - _e orbit * cos _E)
                            -- in orbital coordinates
                            r_o = _rc *^ makevect (cos _nu) (sin _nu) 0
                            v_o = (sqrt (_mu * _a orbit) / _rc) *^ makevect (- sin _E) (sqrt (1 - _e orbit ** 2) * cos _E) 0
                            -- transform to inertial frame
                            _R_Omega = Q.axisAngle (makevect 0.0 0 1) ( _Omega orbit)
                            _R_i    = Q.axisAngle (makevect 1.0 0 0) ( _i orbit)
                            _R_omega = Q.axisAngle (makevect 0.0 0 1) ( _omega orbit)
                            fullTransform = _R_Omega * _R_i * _R_omega


--atan2       :: Double -> Double -> Double
--atan2 0 y   | y > 0     = pi / 2
--            | y < 0     = -pi / 2
--            | otherwise = error
--atan2 x y   | x > 0     = atan (y/x)
--            | y >= 0 && x < 0   = atan (y/x) + pi
--            | y < 0  && x < 0   = atan (y/x) - pi
--            | otherwise = error








