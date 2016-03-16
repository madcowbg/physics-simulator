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
    draw
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

import Control.Monad.Writer
import Control.Monad.Trans.State

import Physics.Orbit.Freefall

class Drawable d where
    draw        :: d -> Picture

instance Drawable SmallWorld where
    draw (SmallWorld currentTime crafts ground gravity controls)
                = pictures (drawForce ground:map (color blue . draw) crafts
                        ++ map (drawOrient . craftCoordinates) crafts
                        ++ map (drawEnergy gravity) crafts
                        ++ [drawCircle target 10])

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
            = pictures ([drawRelativeToCraft craft $ pictures (drawCenter craft:map draw parts)]
                        ++ [drawVelocity craftPlace craftVel]
                        ++ map (color red . drawAction) (partsActions rocket (Tick 1.0 ))
                        ++ map (color green . drawLocalVelocities coordinates craftVel) parts
                        ++ [drawCraftDescription coordinates (inertiaTensor rocket)]
                        ++ [color (light blue) $ drawOrbit 300 coordinates ]
                        ++ [color (light green) $ drawOrbitY 100 (IState (interceptPos + bodyCenter) interceptVel)]
                        ++ [color (dark green) $ drawOrbitY (-100) (IState (progradeInterceptPos + bodyCenter) progradeInterceptVel)]
                        )-- ++ map (color (dark green) . drawAction)) (thrustActions rocket (Tick 1.0 ))
              where (StateTriplet craftPlace craftVel) = stateFrom coordinates (StateTriplet origin atrest)
                    IState interceptPos interceptVel = calculateSigleStepNeededVelocity body
                                        (IState (craftPlace - bodyCenter) craftVel) (target - bodyCenter) (5)
                    IState progradeInterceptPos progradeInterceptVel = calculateSigleStepProgradeBurn body
                                        (IState (craftPlace - bodyCenter) craftVel) (target - bodyCenter)

--calculateSigleStepNeededVelocity    :: CelestialBody -> IState -> Place -> Angle -> IState

target = (- makevect 400 0 200)

bodyOffset = 1000000
bodyCenter = makevect 0 0 (-bodyOffset)
body = CelestialBody (5 * (bodyOffset ** 2))

drawOrbit           :: (EmbeddedFrameOfReference f) => Float -> f -> Picture
drawOrbit offset system    = let
                        (StateTriplet place vel) = stateFrom system (StateTriplet origin atrest)
                      in drawOrbitZ offset place vel

drawOrbitY          :: Float -> IState -> Picture
drawOrbitY offset (IState place vel)
                    = drawOrbitZ offset place vel

drawOrbitZ          :: Float -> Place -> Velocity -> Picture
drawOrbitZ offset place vel = let
                        orbit = fromStateToOrbit body (IState (place - bodyCenter) vel)
                        arguments = map (/ 1) [-20..20]
                        pts = map (fromOrbitToState body orbit) arguments
                        centeredPts = map ((+ bodyCenter) . position) pts
                        currPt = (+ bodyCenter) . position $ fromOrbitToState body orbit 0
                      in pictures [line (map ptPlaceCoord centeredPts),
                                 writeOrbitDescription offset orbit,
                                 translate (-380) (200) $ scale textSize textSize
                                 $ appendLine ("behind pt: (" ++ showFixed (xcoord (head centeredPts)) ++ ", " ++ showFixed (ycoord (head centeredPts)) ++ ", "  ++ showFixed (zcoord (head centeredPts)) ++ ") ") blank,
                                 color red $ drawCircle currPt 5]

writeOrbitDescription :: Float -> Orbit -> Picture
writeOrbitDescription offset (Orbit (OrbitalParams _a _e _i _omega _Omega) _nu _M)
                        =  translate (-offset-80) 300 $ scale textSize textSize
                        $ appendLine ("_a = " ++ showFixed _a)
                        $ appendLine ("_e = " ++ showFixedHighPrecision _e)
                        $ appendLine ("_i = " ++ showFixedHighPrecision _i)
                        $ appendLine ("_omega = " ++ showFixedHighPrecision _omega)
                        $ appendLine ("_Omega = " ++ showFixedHighPrecision _Omega)
                        $ appendLine ("_nu = " ++ showFixedHighPrecision _nu)
                        $ appendLine ("_M = " ++ showFixedHighPrecision _M) blank

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
    draw obj        = let place = objPlace obj
                      in pictures [line [(0,0), (double2Float (xcoord place), double2Float (zcoord place))], drawCircle place 3]

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

textSize = 0.075

drawCraftDescription :: RotatingCoordinates -> InertiaTensor -> Picture
drawCraftDescription (RotatingCoordinates _ (InertialCoordinates location velocity orientation) angularVelocity) mom
                     = translate (-380) (100) $ scale textSize textSize
                        $ appendLine ("coordinates: (" ++ showFixed (xcoord location) ++ ", " ++ showFixed (zcoord location) ++ ") ")
                        $ appendLine ("velocity: (" ++ showFixed (xcoord velocity) ++ ", " ++ showFixed (zcoord velocity) ++ ")")
                        $ appendLine ("3D coordinates: (" ++ showFixed (xcoord location) ++ ", " ++ showFixed (ycoord location) ++ ", " ++ showFixed (zcoord location) ++")")
                        $ appendLine ("3D velocity: (" ++ showFixed (xcoord velocity) ++ ", " ++ showFixed (ycoord velocity) ++ ", " ++ showFixed (zcoord velocity) ++")")
                        $ appendLine ("3D angularVelocity: (" ++ showFixed (xcoord angularVelocity) ++ ", " ++ showFixed (ycoord angularVelocity) ++ ", " ++ showFixed (zcoord angularVelocity) ++")")
                        $ appendLine ("mominertia: " ++ showFixed (det33 mom))
                        $ blank

drawEnergy           :: Gravity -> Rocket -> Picture
drawEnergy gravity rocket
                    = translate (-380) (-100) $ scale textSize textSize
                        $ appendLine ("potential energy: " ++ showFixed potential)
                        $ appendLine ("kinetic energy: " ++ showFixed kinetic)
                        $ appendLine ("potential energy: " ++ showFixed (potential + kinetic)) blank
                    where potential = calcPotential gravity rocket
                          kinetic = calcKinetic rocket

showFixed f = showFFloat (Just 2) f ""
showFixedHighPrecision f = showFFloat (Just 10) f ""


appendLine        :: String -> Picture -> Picture
appendLine txt picture = pictures [translate 0 (-150) picture, text txt]



