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
import Physics.Objects
import Physics.AbstractObjects
import Physics.Forces
import Physics.AbstractForces
import Physics.Time
import Physics.World
import Physics.Craft.Rigid

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Color

import GHC.Float

class Drawable d where
    draw        :: d -> Picture

instance Drawable SmallWorld where
    draw (SmallWorld crafts ground) = pictures (drawForce ground:map (color blue . draw) crafts ++ map drawOrient crafts)

drawOrient      :: RigidCraft -> Picture
drawOrient craft@(RigidCraft parts (PlaceState place _) (RotationState orientation _) _)
                = rotate (angle orientation) $ line [(0,0), (0, 100 * (double2Float (det33 orientation)))]

angle orientation = -radToDeg (argV pt)
                    where acted = orientVector orientation (makevect 1.0 0 0)
                          pt = (double2Float (xcoord acted), double2Float(zcoord acted))

drawAction                              :: ForceAction -> Picture
drawAction (ForceAction place forceAmt) = drawVector place forceAmt

--drawAction (ShockAction place _)        = drawCircle place 0.2 TODO draw craft shocks

class DrawableForce f where
    drawForce       :: f -> Picture

instance DrawableForce StickingGround where
    drawForce (StickingGround zlim)
                    = drawRectangle 0 (zlim-15) 1200 30

instance DrawableForce BouncingGround where
    drawForce (BouncingGround zlim)
                    = drawRectangle 0 (zlim-15) 1200 30


instance Drawable RigidCraft where
    draw craft@(RigidCraft parts placeState (RotationState orientation _) ground)
            = pictures ([drawRelativeToCraft craft $ pictures (drawCenter craft:map draw parts)]
                        ++ [drawVelocity placeState]
                        ++ map (color red . drawAction) (partsActions (Tick 1.0) craft))

drawVelocity (PlaceState place velocity)
            = color blue $ drawVector place velocity


drawRelativeToCraft         :: RigidCraft -> Picture -> Picture
drawRelativeToCraft (RigidCraft parts (PlaceState place vel) (RotationState orientation _) ground) p
    = translateD (xcoord place) (zcoord place) $ rotate (angle orientation) p

drawCenter craft        = line [(-10, -15), (0, 15), (10,-15), (-10,-15)]

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

