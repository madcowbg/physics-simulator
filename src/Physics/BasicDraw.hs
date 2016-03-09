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
                        ++ [drawVelocity (globalState coordinates (atrest, origin))]
                        ++ map (color red . drawAction) (partsActions rocket (Tick 1.0 ))
                        ++ [drawCraftDescription coordinates (momentOfInertia rocket)]
                        )-- ++ map (color (dark green) . drawAction)) (thrustActions rocket (Tick 1.0 ))

drawVelocity (place, velocity)
            = color blue $ drawVector place velocity


drawRelativeToCraft         :: RigidCraft -> Picture -> Picture
drawRelativeToCraft (RigidCraft parts coordinates ground) p
                            = translateD (xcoord place) (zcoord place) $ rotate (angle coordinates) p
                              where place = globalPlace coordinates origin

drawCenter (RigidCraft parts _ _)
                            = line (map ptCoord (parts ++ [head parts]))--[(-10, -15), (0, 15), (10,-15), (-10,-15)]

ptCoord p = (double2Float $ xcoord (objPlace p), double2Float $ zcoord $ objPlace p)

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

drawCraftDescription :: CoordinateSystem -> InertiaMatrix -> Picture
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

appendLine        :: String -> Picture -> Picture
appendLine txt picture = pictures [translate 0 (-150) picture, text txt]
