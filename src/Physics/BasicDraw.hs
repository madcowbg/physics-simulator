-----------------------------------------------------------------------------
--
-- Module      :  Physics.BasicDraw
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
{-# LANGUAGE FlexibleContexts #-}
module Physics.BasicDraw (
    drawOnScreen
) where

import Linear.Matrix

import Physics.Primitives
import Physics.Coordinates.FrameOfReference
import Physics.Coordinates.Inertial
import Physics.Coordinates.Rotating
import Physics.Elementary
import Physics.Craft.Basic
import Physics.Forces
import Physics.AbstractForces
import Physics.Time
import Physics.World.Small
import Physics.Craft.Rigid
import Physics.Craft.Rocket
import Physics.Energy

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Color

import GHC.Float
import Numeric

import Control.Monad.Writer.Lazy
import Control.Monad.Trans.State

import Physics.Orbit.Freefall

import Debug.Trace


textSize = 0.075

drawOnScreen    :: SmallWorld -> Picture
drawOnScreen w  = let (worldPicture, log) = runWriter (draw w)
                    in pictures [worldPicture, printLog (-380) 400 textSize blue log]

class Drawable d where
    draw        :: d -> Writer ConsoleLog Picture

instance Drawable SmallWorld where
    draw (SmallWorld currentTime crafts ground gravity controls)
                = do
                    craftsPicture <- mapM draw crafts
                    mapM_ (drawEnergy gravity) crafts
                    return (pictures (drawForce ground:craftsPicture
                            ++ map (drawOrient . craftCoordinates) crafts
                            ++ [drawCircle target 10]))

drawOrient      :: (EmbeddedFrameOfReference f) => f -> Picture
drawOrient coordinates
                = rotate (angle coordinates) $ line [(0,0), (0, 100)]

angle           :: (EmbeddedFrameOfReference f) => f -> Float
angle coordinates = -radToDeg (argV pt)
                    where acted = directionFrom coordinates (makevect 1.0 0 0)
                          pt = (double2Float (xcoord acted), double2Float(zcoord acted))

drawAction                              :: ForceAction -> Picture
drawAction (ForceAction place forceAmt) = drawVector place forceAmt

class DrawableForce f where
    drawForce       :: f -> Picture

instance DrawableForce StickingGround where
    drawForce (StickingGround zlim)
                    = drawRectangle 0 (zlim-15) 1200 30

instance DrawableForce BouncingGround where
    drawForce (BouncingGround zlim)
                    = drawRectangle 0 (zlim-15) 1200 30


instance Drawable Rocket where
    draw rocket@(Rocket craft@(RigidCraft parts coordinates ground) thrusters)
            = --traceShow (startVelocity interceptStop) $
                do
                    tell ["", "Craft description:"]
                    printCraftDescription coordinates (inertiaTensor rocket)
                    tell ["", "Current rocket trajectory:"]
                    rocketTrajectory <- drawOrbit 300 coordinates
--                    tell ["", "Intercept Burn Trajectory:"]
--                    interceptBurnTrajectory <- drawBurn 100 interceptBurn
--                    tell ["", "Intercept STOP Trajectory:"]
--                    interceptStopTrajectory <- drawBurn (-100) interceptStop
                    tell ["", "Prograde Intercept Burn Trajectory:"]
                    progradeInterceptBurnTrajectory <- drawBurn (-300) progradeInterceptBurn
                    tell ["", "Prograde Intercept Stop Trajectory:"]
                    progradeInterceptStopTrajectory <- drawBurn (-500) progradeInterceptStop
                    partsPictures <- mapM draw parts
                    return $ color blue $ pictures ([drawRelativeToCraft craft $ pictures (drawCenter craft:partsPictures)]
                        ++ [drawVelocity craftPlace craftVel]
                        ++ map (color red . drawAction) (partsActions rocket (Tick 1.0 ))
                        ++ map (color green . drawLocalVelocities coordinates craftVel) parts
                        ++ [color (light blue) rocketTrajectory ]
--                        ++ [color (light green) interceptBurnTrajectory]
--                        ++ [color (light yellow) interceptStopTrajectory]
                        ++ [color (dark green) progradeInterceptBurnTrajectory]
                        ++ [color (dark yellow) progradeInterceptStopTrajectory]
                        )-- ++ map (color (dark green) . drawAction)) (thrustActions rocket (Tick 1.0 ))
              where (StateTriplet craftPlace craftVel) = stateFrom coordinates (StateTriplet origin atrest)
                    interceptBurn = head $ calculateSigleStepNeededVelocity body
                                        (pvRelToBody craftPlace craftVel) (pRelToBody target) (5)
                    interceptStop = calculateStopBurn body (calcEndOrbit body interceptBurn) (pRelToBody target)
                    progradeInterceptBurn = head $ calculateSigleStepProgradeBurn body
                                        (IState (craftPlace - bodyCenter) craftVel) (pRelToBody target)
                    progradeInterceptStop = calculateStopBurn body (calcEndOrbit body progradeInterceptBurn) (pRelToBody target)



--calculateSigleStepNeededVelocity    :: CelestialBody -> IState -> Place -> Angle -> IState

-- TODO REMOVE
pvRelToBody  :: Place -> Velocity -> IState
pvRelToBody craftPlace = IState (craftPlace - bodyCenter)

pRelToBody  :: Place -> Place
pRelToBody x = x - bodyCenter

target = - makevect 400 0 200

bodyOffset = 1000000
bodyCenter = makevect 0 0 (-bodyOffset)
body = CelestialBody (5 * (bodyOffset ** 2))

drawBurn           :: Float -> ScheduledBurn -> Writer ConsoleLog Picture
drawBurn offset burn
                    = do
                        orbitPicture <- drawOrbitY offset (IState (endPosition burn + bodyCenter) (endVelocity burn + makevect 0.0001 0 0))
                        tell ["dV = " ++ showFixed (calcDeltaV burn)]
                        return orbitPicture


drawOrbit           :: (EmbeddedFrameOfReference f) => Float -> f -> Writer ConsoleLog Picture
drawOrbit offset system    = let
                        (StateTriplet place vel) = stateFrom system (StateTriplet origin atrest)
                      in drawOrbitZ offset place vel

drawOrbitY          :: Float -> IState -> Writer ConsoleLog Picture
drawOrbitY offset (IState place vel)
                    = drawOrbitZ offset place vel

drawOrbitZ          :: Float -> Place -> Velocity -> Writer ConsoleLog Picture
drawOrbitZ offset place vel =
                    do
                        printOrbitDescription offset orbit
                        return $ pictures [line (map ptPlaceCoord centeredPts),
                                 color red $ drawCircle currPt 5]
                    where
                        orbit = fromStateToOrbit body (IState (place - bodyCenter) vel)
                        arguments = map (/ 1) [-20..20]
                        pts = map (fromOrbitToState body orbit) arguments
                        centeredPts = map ((+ bodyCenter) . position) pts
                        currPt = (+ bodyCenter) . position $ fromOrbitToState body orbit 0

printOrbitDescription :: Float -> Orbit -> Writer ConsoleLog ()
printOrbitDescription offset (Orbit (OrbitalParams _a _e _i _omega _Omega) _nu _M)
                        = tell ["_a = " ++ showFixed _a,
                                "_e = " ++ showFixedHighPrecision _e,
                                "_i = " ++ showFixedHighPrecision _i,
                                "_omega = " ++ showFixedHighPrecision _omega,
                                "_Omega = " ++ showFixedHighPrecision _Omega,
                                "_nu = " ++ showFixedHighPrecision _nu,
                                "_M = " ++ showFixedHighPrecision _M]

drawLocalVelocities :: (EmbeddedFrameOfReference f) => f -> Velocity -> RigidPointObj -> Picture
drawLocalVelocities system craftVel obj
                    = let (StateTriplet place vel) = stateFrom system (StateTriplet (objPlace obj) atrest)
                      in drawVector place (5 * (vel - craftVel))

drawVelocity place velocity
            = color blue $ drawVector place velocity


drawRelativeToCraft         :: RigidCraft -> Picture -> Picture
drawRelativeToCraft (RigidCraft parts coordinates ground) p
                            = translateD (xcoord place) (zcoord place) $ rotate (angle coordinates) p
                              where place = placeFrom coordinates origin

drawCenter (RigidCraft parts _ _)
                            = line (map ptCoord (parts ++ [head parts]))--[(-10, -15), (0, 15), (10,-15), (-10,-15)]

ptCoord p = ptPlaceCoord (objPlace p)
ptPlaceCoord place = (double2Float $ xcoord place, double2Float $ zcoord $ place)

instance Drawable RigidPointObj where
    draw obj        = return $ pictures [line [(0,0), (double2Float (xcoord place), double2Float (zcoord place))], drawCircle place 3]
                        where place = objPlace obj

translateD          :: Double -> Double -> Picture -> Picture
translateD x y      = translate (double2Float x) (double2Float y)

drawCircle          :: Place -> Double -> Picture
drawCircle place size = translateD (xcoord place) (zcoord place) $ circle (double2Float size)

drawArrow           :: Double -> Double -> Double -> Double -> Picture
drawArrow x y xvel yvel = translateD x y $ line [(0.0,0.0), (double2Float xvel, double2Float yvel)]

drawRectangle       :: Double -> Double -> Double -> Double -> Picture
drawRectangle cx cy width height = translateD cx cy $ color (dark green) $ rectangleSolid (double2Float width) (double2Float height)

drawVector          :: Place -> Velocity -> Picture
drawVector place vel = drawArrow (xcoord place) (zcoord place) (xcoord vel) (zcoord vel)


printCraftDescription :: RotatingCoordinates -> InertiaTensor -> Writer ConsoleLog ()
printCraftDescription (RotatingCoordinates _ (InertialCoordinates location velocity orientation) angularVelocity) mom
                     = do
                        tell ["coordinates: (" ++ showFixed (xcoord location) ++ ", " ++ showFixed (zcoord location) ++ ") "]
                        tell ["velocity: (" ++ showFixed (xcoord velocity) ++ ", " ++ showFixed (zcoord velocity) ++ ")"]
                        tell ["3D coordinates: (" ++ showFixed (xcoord location) ++ ", " ++ showFixed (ycoord location) ++ ", " ++ showFixed (zcoord location) ++")"]
                        tell ["3D velocity: (" ++ showFixed (xcoord velocity) ++ ", " ++ showFixed (ycoord velocity) ++ ", " ++ showFixed (zcoord velocity) ++")"]
                        tell ["3D angularVelocity: (" ++ showFixed (xcoord angularVelocity) ++ ", " ++ showFixed (ycoord angularVelocity) ++ ", " ++ showFixed (zcoord angularVelocity) ++")"]
                        tell ["mominertia: " ++ showFixed (det33 mom)]

drawEnergy           :: Gravity -> Rocket -> Writer ConsoleLog ()
drawEnergy gravity rocket
                    = do
                        tell ["potential energy: " ++ showFixed potential]
                        tell ["kinetic energy: " ++ showFixed kinetic]
                        tell ["potential energy: " ++ showFixed (potential + kinetic)]
                      where
                        potential = calcPotential gravity rocket
                        kinetic = calcKinetic rocket

showFixed f = showFFloat (Just 2) f ""
showFixedHighPrecision f = showFFloat (Just 10) f ""


--appendLine        :: String -> Picture -> Picture
--appendLine txt picture = pictures [translate 0 (-150) picture, text txt]


type ConsoleLog = [String]

printLog    :: Float -> Float -> Float -> Color -> ConsoleLog -> Picture
printLog xoff yoff textSize textColor lines
            = color textColor $ translate xoff yoff $ scale textSize textSize
                $ pictures (printall 0 lines)

printall        :: Float -> ConsoleLog -> [Picture]
printall offset [] = []
printall offset (line:lines) = translate 0 (-offset) (text line) : printall (offset + 150) lines
