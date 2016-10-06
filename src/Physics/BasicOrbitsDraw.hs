-----------------------------------------------------------------------------
--
-- Module      :  Physics.BasicOrbitsDraw
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

module Physics.BasicOrbitsDraw (
    drawOrbitsOnScreen
) where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Color

import Control.Monad.Writer.Lazy

import Physics.DrawUtils

import Physics.Primitives
import Physics.Elementary

import Physics.Orbit.Freefall
import Debug.Trace

drawOrbitsOnScreen    :: Picture
drawOrbitsOnScreen = let (satPics, log) = runWriter iterateOverOrbits
                        in (pictures [drawPlanet, satPics, printLog (-380) 200 0.08 blue log])

iterateOverOrbits   :: Writer ConsoleLog Picture
iterateOverOrbits   = do
                        sat1Pic <- drawOrbit satPlace satVel1
                        --sat2Pic <- drawOrbit satPlace satVel2
                        --sat3Pic <- drawOrbit satPlace satVel3
                        return (pictures [sat1Pic, --sat2Pic, sat3Pic,
                                          drawExactPlace])

bodyCenter = makevect 10 0 10
body = CelestialBody (6 * (norm bodyCenter ** 2))

satPlace = makevect (-10) 0 0
satVel1 = makevect 0 0 (-10)
satVel2 = makevect 0 0 (-5)
satVel3 = makevect 0 0 15


drawPlanet  :: Picture
drawPlanet  = color green $ drawCircle bodyCenter 3

drawExactPlace = color yellow $ drawCircle satPlace 2

--drawSatelite
--     drawPlace & velocity
--drawOrbit
--drawPeriapsis vector
--drawEllipseCenter
--drawElliptic vector & angle
--draw place after epoch
--draw place&velocity at attitudes +pi/2, +pi, +1.5pi
--

drawOrbit          :: Place -> Velocity -> Writer ConsoleLog Picture
drawOrbit place vel =
                    do
                        printOrbitDescription orbit
                        return $ pictures [line (map ptPlaceCoord centeredPts),
                                 color red $ drawCircle currPt 5]
                    where
                        orbit = fromStateToOrbit body (IState (place - bodyCenter) vel) 0
                        arguments = map (/ 3) [-200..200] -- TODO
                        pts = map (fromOrbitToState orbit) arguments
                        centeredPts = map ((+ bodyCenter) . position) pts
                        currPt = (+ bodyCenter) . position $ fromOrbitToState orbit 0

printOrbitDescription :: Orbit -> Writer ConsoleLog ()
printOrbitDescription (Orbit (OrbitalParams _a _e _i _omega _Omega body) anomaly epochAtPeriapsis)
                        = tell ["=====================================================",
                                "_a = " ++ showFixed _a,
                                "_e = " ++ showFixedHighPrecision _e,
                                "_i = " ++ showFixedHighPrecision _i,
                                "_omega = " ++ showFixedHighPrecision _omega,
                                "_Omega = " ++ showFixedHighPrecision _Omega,
                                "_nu = " ++ showFixedHighPrecision (angleTrue anomaly),
                                "_M = " ++ showFixedHighPrecision (angleMean anomaly),
                                "_epoch = " ++ showFixedHighPrecision epochAtPeriapsis]
