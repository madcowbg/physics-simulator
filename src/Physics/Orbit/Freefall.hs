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
    calculateSigleStepNeededVelocity,
    calculateSigleStepProgradeBurn,
    --- move
    ScheduledBurn, endVelocity, endPosition, startVelocity,
    calcEndOrbit, calcTotalDeltaV, calcDeltaV,
    calculateStopBurn,
    --- remove
    params,
    --- flight planning
    ExecuteBurn (ExecuteBurn),
    FlightPlan (FlightPlan),
    --
    Anomaly, angleTrue, angleMean, angleEccentric,
    fromTrue, fromMean, fromEpoch,
    addTrue, addMean, addTime,

) where

import qualified Linear.Quaternion as Q -- TODO move to primitives
import Linear.Matrix -- TODO move to primitives
import Linear.Metric -- TODO move to primitives
import Linear.Vector -- TODO move to primitives
import Linear.V3     -- TODO move to primitives
import Control.Lens

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
                    _Omega  :: Angle, -- longitude of ascending node
                    body    :: CelestialBody}

----------------------
data Anomaly = Anomaly {orbitalParams :: OrbitalParams, angleTrue, angleMean , angleEccentric :: Angle}

createAnomaly :: (Angle->Angle) -> (Angle->Angle) -> (Angle->Angle) -> OrbitalParams -> Angle -> Anomaly
createAnomaly f g h params value
                | value < 0 || value >= 2*pi   =createAnomaly f h g params (normalizeAngle value)
                | otherwise                    = Anomaly params (f value) (g value) (h value)

trueToEccentricAnomaly
                    :: OrbitalParams -> Double -> Double
trueToEccentricAnomaly orbit _nu
                    = 2 * atan (tan (_nu / 2) / sqrt ((1 + _e orbit)/(1-_e orbit)))

eccentricToMeanAnomaly       :: OrbitalParams -> Double -> Double
eccentricToMeanAnomaly orbit __E = __E - _e orbit * sin __E

trueToMeanAnomaly   :: OrbitalParams -> Double -> Double
trueToMeanAnomaly orbit =  eccentricToMeanAnomaly orbit . trueToEccentricAnomaly orbit

meanToEccentricAnomaly :: OrbitalParams -> Angle -> Angle
meanToEccentricAnomaly orbitalParams _M_t = newton 1e-4 maxIter f_E f_E' _M_t                -- numerical solve for eccentric anomaly
                                where
                                    f_E _E  = _E - _e orbitalParams * sin _E - _M_t
                                    f_E' _E = 1 - _e orbitalParams * cos _E

eccentricToTrueAnomaly     :: OrbitalParams -> Angle -> Angle
eccentricToTrueAnomaly orbitalParams _E
            = 2 * atan2 (sqrt (1+_e orbitalParams) * sin (_E/2)) (sqrt (1-_e orbitalParams) * cos (_E/2))

meanToTrueAnomaly       :: OrbitalParams -> Angle -> Angle
meanToTrueAnomaly orbitalParams = eccentricToTrueAnomaly orbitalParams . meanToEccentricAnomaly orbitalParams

-- constructors
fromTrue     :: OrbitalParams -> Angle -> Anomaly
fromTrue orbit = createAnomaly id (trueToMeanAnomaly orbit) (trueToEccentricAnomaly orbit) orbit

fromMean     :: OrbitalParams -> Angle -> Anomaly
fromMean orbit = createAnomaly (meanToTrueAnomaly orbit) id (meanToEccentricAnomaly orbit) orbit

fromEpoch     :: OrbitalParams -> Time -> Anomaly
fromEpoch orbit time = fromMean orbit (2 * pi * time / orbitalPeriod orbit)

-- operations
addTrue     :: Anomaly -> Angle -> Anomaly
addTrue anomaly angle = fromTrue (orbitalParams anomaly) (angleTrue anomaly + angle)

addMean      :: Anomaly -> Angle -> Anomaly
addMean anomaly angle = fromMean (orbitalParams anomaly) (angleMean anomaly + angle)

addTime      :: Anomaly -> Time -> Anomaly
addTime anomaly time = fromEpoch (orbitalParams anomaly) (timeEpoch anomaly + time)

timeEpoch      :: Anomaly -> Time
timeEpoch anomaly = (angleMean anomaly / 2*pi) * orbitalPeriod (orbitalParams anomaly)

----------------------

data Orbit = Orbit {params :: OrbitalParams,
                    anomaly :: Anomaly,
                    periapsisEpoch :: Time}

--trueAnomaly     = _nu
--meanAnomaly     = _M

gravityConst   = 1 -- NOT 9.80665 -- m/s^2


data IState = IState {position :: Place, -- place in ECI
                      velocity :: Velocity} -- velocity in ECI

data CelestialBody = CelestialBody {mass :: Double} --WTF?

-- algorithm from http://space.stackexchange.com/questions/1904/how-to-programmatically-calculate-orbital-elements-using-position-velocity-vecto
-- alt: https://downloads.rene-schwarz.com/download/M002-Cartesian_State_Vectors_to_Keplerian_Orbit_Elements.pdf

-- polar vector
_Khat              = makevect 0 0 1

fromStateToOrbitalParams    :: CelestialBody -> IState -> Time -> OrbitalParams
fromStateToOrbitalParams body state epoch = params (fromStateToOrbit body state epoch)

fromStateToOrbit :: CelestialBody -> IState -> Time -> Orbit
fromStateToOrbit body state@(IState r v) epoch
                    = Orbit {
                            params = _params,                                   -- longitude of ascending node
                            anomaly = _anomaly,
                            periapsisEpoch = epoch - timeEpoch _anomaly}                -- epoch of last periapsis
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
                            _earr   = eccentricityVector r v _harr _mu
                            __e      = norm _earr
                            -- specific mechanical energy
                            _E      = quadrance v / 2 - _mu / _r
                            __a     = - _mu / (2 * _E)
                            _params = OrbitalParams {
                                _a     = __a,                                   -- semi-major axis
                                _e     = __e,                                   -- eccentricity
                                _i     = acos (_harr ^. _z / norm _harr),       -- inclination
                                _omega = calcArgumentOfPeriapsis _earr _z _nhat,-- argument of periapsis
                                _Omega = acos (_nhat ^._x / _n),
                                body = body}
                            _anomaly = fromTrue _params (calcTrueAnomaly _earr state)

orbitalPeriodFromParams :: Distance -> Double -> Double
orbitalPeriodFromParams _a _mu      = 2 * pi * sqrt ((_a ** 3) / _mu)

orbitalPeriod       :: OrbitalParams -> Double
orbitalPeriod orbit = orbitalPeriodFromParams (_a orbit) (_mubody $ body orbit)

eccentricityVector  :: Place -> Velocity -> Vector3 -> Double -> Vector3
eccentricityVector r v _harr _mu
                    = cross v _harr ^/ _mu - (r ^/ norm r)

angularMomentum     :: IState -> Vector3
angularMomentum (IState r v) = cross r v

calcArgumentOfPeriapsis _earr _z _nhat
                    | _earr ^. _z < 0   = 2 * pi - _omega
                    | otherwise         = _omega
                    where _omega  = acos (dot _nhat _earr / (norm _nhat * norm _earr))

calcTrueAnomaly     :: Vector3 -> IState -> Double
calcTrueAnomaly _earr (IState r v)
                    = calcSignedAngle _earr r v

calcSignedAngle     :: Vector3 -> Vector3 -> Vector3 -> Double
calcSignedAngle base next velocity
                    = calcHandedAngle base next (dot next velocity)

calcHandedAngle     :: Vector3 -> Vector3 -> Double -> Double
calcHandedAngle base next handedness
                    | handedness < 0  = 2 * pi - _nu
                    | otherwise    = _nu
                    where _nu = acos (dot base next / (norm base * norm next))

_mubody body    = gravityConst * mass body

-- algorithm from https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf
newton epsilon iter f f' guess
            | iter < 0          = guess
            | err < epsilon     = newGuess
            | otherwise         = newton epsilon (iter - 1) f f' newGuess
            where newGuess = guess - (f guess / f' guess)
                  err =  abs (newGuess - guess)

--epochToMeanAnomaly   :: Orbit -> Time -> Angle
--epochToMeanAnomaly orbit time = deltaTimeFromMeanAnomalyToMeanAnomaly (params orbit) 0 (time - periapsisEpoch orbit)
--
--deltaTimeToMeanAnomaly   :: Orbit -> Time -> Angle
--deltaTimeToMeanAnomaly orbit = deltaTimeFromMeanAnomalyToMeanAnomaly (params orbit) (_M orbit)

--deltaTimeFromMeanAnomalyToMeanAnomaly   :: OrbitalParams -> Angle -> Time -> Angle
--deltaTimeFromMeanAnomalyToMeanAnomaly orbitalParams _M_0 deltaT
--                    = normalizeAngle $ _M_0 + deltaT * sqrt (_mu / (_a orbitalParams ** 3))
--                        where
--                            _mu     = _mubody (body orbitalParams)

fromOrbitToState    :: Orbit -> Time -> IState
fromOrbitToState orbit deltaT
                    = fromOrbitAndAnomalyToState (params orbit) (addTime (anomaly orbit) deltaT)

fromOrbitAndAnomalyToState  :: OrbitalParams -> Anomaly -> IState
fromOrbitAndAnomalyToState orbitalParams anomaly
                    = rotateToBodyInertial orbitalParams (IState r_o v_o) -- transform to inertial frame
                      where
                            _mu = _mubody (body orbitalParams)
                            _nu = angleTrue anomaly
                            _E = angleEccentric anomaly
                            -- distance to central body
                            _rc = _a orbitalParams * (1 - _e orbitalParams * cos _E)
                            -- in orbital coordinates
                            r_o = _rc *^ makevect (cos _nu) (sin _nu) 0
                            v_o = (sqrt (_mu * _a orbitalParams) / _rc) *^ makevect (- sin _E) (sqrt (1 - _e orbitalParams ** 2) * cos _E) 0

maxIter = 100

rotateToBodyInertial    :: OrbitalParams -> IState -> IState
rotateToBodyInertial orbitalParams (IState r_o v_o)
                        = IState {position = Q.rotate fullTransform r_o, velocity = Q.rotate fullTransform v_o}
                          where _R_Omega = Q.axisAngle (makevect 0.0 0 1) ( _Omega orbitalParams)
                                _R_i    = Q.axisAngle (makevect 1.0 0 0) ( _i orbitalParams)
                                _R_omega = Q.axisAngle (makevect 0.0 0 1) ( _omega orbitalParams)
                                fullTransform = _R_Omega * _R_i * _R_omega


-- | Normalize an angle to be between 0 and 2*pi radians
normalizeAngle :: Double -> Double
normalizeAngle = normalizePeriod (2 * pi)

normalizePeriod:: Double -> Double -> Double
normalizePeriod period f = f - period * floor' (f / period)
 where  floor' :: Double -> Double
        floor' x = fromIntegral (floor x :: Int)
{-# INLINE normalizeAngle #-}


-- now this gets interesting
--type DeltaV = Vector3

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

projectOnto         :: Vector3 -> Vector3 -> Vector3
projectOnto n vec   = vec - (dot n vec / quadrance n) *^ n

calculateSigleStepProgradeBurn    :: CelestialBody -> IState -> Double -> Place -> [ScheduledBurn]
calculateSigleStepProgradeBurn  body state@(IState position v) burnStart targetPlace
            = calculateSigleStepNeededVelocity body state targetPlace (calcSignedAngle position v v) burnStart

calculateSigleStepNeededVelocity    :: CelestialBody -> IState -> Place -> Angle -> Double -> [ScheduledBurn]
calculateSigleStepNeededVelocity body state@(IState position v) targetPlace alpha burnStart
            = let
                _OX = targetPlace
                _OA = position
                --deltaTrueAnomaly = acos (dot _OA _OX / (norm _OA * norm _OX))
                _OAunit = _OA ^/ norm _OA
                vproj = projectOnto (cross _OA _OX) v
                _harr = cross _OA vproj  -- needed normal of intercept trajectory (specific angular momentum)
                _hunit = _harr ^/ norm _harr
                direction = Q.rotate (Q.axisAngle _hunit alpha) _OAunit
                beta = calcSignedAngle _OA _OX direction
                optV = univariateMin (-600) 600 (\speed -> norm (_OX - predictAtAnomaly body _OA _OX _harr beta direction speed))
              in [ScheduledBurn 0 state (IState _OA (optV *^ direction))]
              where
                predictAtAnomaly    :: CelestialBody -> Vector3 -> Vector3 -> Vector3 -> Angle -> Vector3 -> Double -> Place
                predictAtAnomaly body _OA _OX _harr beta direction speed
                                    = let
                                        _AB = speed *^ direction  -- predicted velocity
                                        interceptOrbit = fromStateToOrbit body (IState _OA _AB) burnStart
                                        _mu     = _mubody body
                                        _earr = eccentricityVector _OA _AB _harr _mu
                                        -- ASSERT calcSignedAngle _earr _OA == _nu interceptOrbit
                                        _anomaly_t = addTrue (anomaly interceptOrbit) beta -- with proper handedness! (direction of rotation)
                                        IState ipos ivel = fromOrbitAndAnomalyToState (params interceptOrbit) _anomaly_t
                                      in ipos
--                                      traceShow (calcSignedAngle _earr _OA _AB - _nu interceptOrbit) $

univariateMin      :: Double -> Double -> (Double -> Double) -> Double
univariateMin min max f = let guide = C.easyOptimize f (min, max) 100 (mkStdGen 0) in C.pt guide


--trueAnomalyToEpoch  :: OrbitalParams -> Angle -> Time
--trueAnomalyToEpoch orbit trueAnomaly
--                    = let meanAnomalyAtBurn = trueToMeanAnomaly (_e orbit) trueAnomaly
--                      in meanAnomalyAtBurn * orbitalPeriod orbit


calculateStopBurn :: OrbitalParams -> Place -> ScheduledBurn
calculateStopBurn orbit direction
                    = let
                        IState periapsis velAtPeriapsis = fromOrbitAndAnomalyToState orbit (fromTrue orbit 0.0)
                        targetAnomaly = fromTrue orbit (calcSignedAngle periapsis direction velAtPeriapsis)
                        stateAtTarget = fromOrbitAndAnomalyToState orbit targetAnomaly
                      in ScheduledBurn (timeEpoch targetAnomaly) stateAtTarget (IState (position stateAtTarget) (makevect 0 0 0))

type DeltaV = Double
data ScheduledBurn = ScheduledBurn {scheduledTime :: Time, startState :: IState, endState :: IState}

endVelocity     :: ScheduledBurn -> Velocity
endVelocity     = velocity . endState
endPosition     = position . endState

startVelocity   :: ScheduledBurn -> Velocity
startVelocity   = velocity . startState

velocityChange      :: ScheduledBurn -> Velocity
velocityChange burn = startVelocity burn - endVelocity burn

calcDeltaV      :: ScheduledBurn -> DeltaV
calcDeltaV      = norm . velocityChange

calcTotalDeltaV :: [ScheduledBurn] -> DeltaV
calcTotalDeltaV burns = sum (map calcDeltaV burns)

calcEndOrbit    :: CelestialBody -> ScheduledBurn -> Orbit
calcEndOrbit body burn
                = fromStateToOrbit body (endState burn) (scheduledTime burn)


data ExecuteBurn= ExecuteBurn {startTime :: Time, endTime :: Time, acceleration :: Acceleration}
data FlightPlan = FlightPlan {current :: Orbit, burns :: [ExecuteBurn]}

prepareBurn     :: OrbitalParams -> ScheduledBurn -> Double -> ExecuteBurn
prepareBurn orbit burn maxAccel
                = let
                    timeToBurn = timeEpoch (fromEpoch orbit (scheduledTime burn))
                    burnTime = calcDeltaV burn / maxAccel
                    acceleration = velocityChange burn ^/ burnTime
                   in ExecuteBurn (timeToBurn - burnTime / 2) (timeToBurn + burnTime / 2) acceleration

midTime    :: ExecuteBurn -> Double
midTime burn = (startTime burn + endTime burn) / 2

burnTime   :: ExecuteBurn -> Double
burnTime burn = endTime burn - startTime burn

totalVelocityChange    :: ExecuteBurn -> Velocity
totalVelocityChange burn = burnTime burn *^ acceleration burn

coast           :: Orbit -> Time -> Orbit
coast orbit time     = orbit {anomaly = fromEpoch (params orbit) time}

evolveToTime     :: FlightPlan -> Time -> FlightPlan
evolveToTime plan@(FlightPlan current burns) time
                | null burns || startTime (head burns) > time
                                =  plan {current = coast current time}
                | otherwise     = evolveToTime (plan {current = orbitAfterFirstBurn, burns = tail burns}) time
                                where
                                    currentBurn = (head burns)
                                    orbitAtBurn = coast current (midTime currentBurn)
                                    IState burnPlace velocity = fromOrbitToState orbitAtBurn 0
                                    newState = IState burnPlace (velocity + totalVelocityChange currentBurn)
                                    orbitAfterFirstBurn = fromStateToOrbit (body (params current)) newState 0


--flightStates    :: FlightPlan -> [Time] -> [IState]
--
