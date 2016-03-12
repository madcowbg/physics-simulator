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
import Physics.Coordinates
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
                        ++ map (drawEnergy gravity) crafts)

drawOrient      :: CoordinateSystem -> Picture
drawOrient coordinates
                = rotate (angle coordinates) $ line [(0,0), (0, 100)]

angle coordinates = -radToDeg (argV pt)
                    where acted = globalOrientation coordinates (makevect 1.0 0 0)
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
                        ++ [drawOrbit coordinates]
                        ++ [drawOrbitY interceptState]
                        )-- ++ map (color (dark green) . drawAction)) (thrustActions rocket (Tick 1.0 ))
              where (craftPlace, craftVel) = globalState coordinates (origin, atrest)
                    interceptState = calculateSigleStepNeededVelocity body
                                        (IState (globalPlace coordinates atrest - bodyCenter) atrest) (makevect 0 0 0) 0

--calculateSigleStepNeededVelocity    :: CelestialBody -> IState -> Place -> Angle -> IState

bodyOffset = 1000000
bodyCenter = makevect 0 0 (-bodyOffset)
body = CelestialBody (5 * (bodyOffset ** 2))

drawOrbit           :: CoordinateSystem -> Picture
drawOrbit system    = let
                        (place, vel) = globalState system (origin, atrest)
                      in drawOrbitZ place vel

drawOrbitY          :: IState -> Picture
drawOrbitY (IState place vel)
                    = drawOrbitZ place vel

drawOrbitZ          :: Place -> Velocity -> Picture
drawOrbitZ place vel = let
                        orbit = fromStateToOrbit body (IState (place - bodyCenter) vel)
                        arguments = map (/ 1) [-20..20]
                        pts = map (fromOrbitToState body orbit) arguments
                        centeredPts = map ((+ bodyCenter) . position) pts
                        currPt = (+ bodyCenter) . position $ fromOrbitToState body orbit 0
                      in pictures [color (light blue) $ line (map ptPlaceCoord centeredPts),
                                 writeOrbitDescription orbit,
                                 translate (-380) (200) $ scale textSize textSize
                                 $ appendLine ("behind pt: (" ++ showFixed (xcoord (head centeredPts)) ++ ", " ++ showFixed (ycoord (head centeredPts)) ++ ", "  ++ showFixed (zcoord (head centeredPts)) ++ ") ") blank,
                                 color red $ drawCircle currPt 5]

writeOrbitDescription :: Orbit -> Picture
writeOrbitDescription (Orbit (OrbitalParams _a _e _i _omega _Omega) _nu _M)
                        =  translate (-380) (300) $ scale textSize textSize
                        $ appendLine ("_a = " ++ showFixed _a)
                        $ appendLine ("_e = " ++ showFixedHighPrecision _e)
                        $ appendLine ("_i = " ++ showFixedHighPrecision _i)
                        $ appendLine ("_omega = " ++ showFixedHighPrecision _omega)
                        $ appendLine ("_Omega = " ++ showFixedHighPrecision _Omega)
                        $ appendLine ("_nu = " ++ showFixedHighPrecision _nu)
                        $ appendLine ("_M = " ++ showFixedHighPrecision _M) blank

drawLocalVelocities :: CoordinateSystem -> Velocity -> RigidPointObj -> Picture
drawLocalVelocities system craftVel obj
                    = let (place, vel) = globalState system (objPlace obj, atrest)
                      in drawVector place (5 * (vel - craftVel))

drawVelocity place velocity
            = color blue $ drawVector place velocity


drawRelativeToCraft         :: RigidCraft -> Picture -> Picture
drawRelativeToCraft (RigidCraft parts coordinates ground) p
                            = translateD (xcoord place) (zcoord place) $ rotate (angle coordinates) p
                              where place = globalPlace coordinates origin

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

drawCraftDescription :: CoordinateSystem -> InertiaTensor -> Picture
drawCraftDescription (CoordinateSystem _ location velocity _ angularVelocity) mom
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


--newtype State s a = State { runState :: s -> (a, s) }

type LineOffset = Float
type ConsoleIO  = State LineOffset Picture

sequentialLog   :: ConsoleIO
sequentialLog   = state (\offset -> (blank, offset))

logText         :: String -> ConsoleIO
logText line    = do
                    offset <- get
                    put (offset + 120)
                    return (translate 0 offset $ text line)

--logText         :: String -> ConsoleIO
--logText string  =

--data ConsoleIO a = ConsoleIO (ConsoleOffset -> (a, ConsoleOffset))
--
--instance Functor ConsoleIO where
--    fmap f (ConsoleIO g) = ConsoleIO (\offset -> let (val, newoffset) = g offset
--                                                 in (f val, newoffset))
--
--instance Applicative ConsoleIO where
--    pure = return
--    (<*>) = ap
--
--instance Monad ConsoleIO where
----    return a = ConsoleIO $ \offset -> (a, offset)
--    ConsoleIO oper >>= nextoper
--        = ConsoleIO (\offset -> let (nextV, newoffset) = oper offset
--                                    ConsoleIO nextf = nextoper nextV
--                                in nextf newoffset)
--
--data ConsoleST  = ST ConsoleOffset Picture
--
--logText         :: String -> ConsoleST
--logText string    = ST 0 (text string)

--simpleConsole   ::

--instance Monad ConsoleIO where

--data ConsoleText = ConsoleText [String]-- (a -> Picture)
--
--instance Monoid ConsoleText where
--    mempty  = ConsoleText []
--    mappend (ConsoleText a) (ConsoleText b) = ConsoleText (a ++ b)

--data Console o = Console o

--instance Functor Console where
--    fmap f (Console = map

--instance Applicative Console where

--instance Monad Console where
--
--instance Writer Picture
--
--instance MonadWriter ConsoleIO
--instance Monoid ConsoleIO where
--  mempty    = ConsoleIO 0 0
--  mappend (ConsoleIO x y) :: m -> m -> m
----
--  --fmap :: (a -> b) -> f a -> f b
--  fmap  = map

--instance Monoid Functor  where
--    ConsoleIO lines `bind` f = ConsoleIO line:lines
--    return line = ConsoleIO [line]

appendLine        :: String -> Picture -> Picture
appendLine txt picture = pictures [translate 0 (-150) picture, text txt]

--logText :: (Show a, MonadWriter [String] m) => a -> m a
--logText text = writer (text, ["Got number: " ++ show text])




