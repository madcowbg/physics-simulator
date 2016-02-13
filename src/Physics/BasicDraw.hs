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

import Physics.Primitives
import Physics.Objects
import Physics.AbstractObjects
import Physics.Forces
import Physics.AbstractForces
import Physics.Time
import Physics.World
import Physics.Craft.Rigid

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import GHC.Float

class Drawable d where
    draw        :: d -> Picture

instance Drawable SmallWorld where
    draw (SmallWorld crafts ground) = pictures (drawForce ground:map (color blue . draw) crafts)

drawAction                              :: Place -> ForceAction -> Picture
drawAction craftPlace (ForceAction place forceAmt) = drawVector (place - craftPlace) forceAmt

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
    draw craft@(RigidCraft parts (PlaceState place vel) rotationState ground)
            = translate (double2Float (xcoord place)) (double2Float (zcoord place))
            $ pictures (drawCenter craft:map draw parts ++ [drawArrow 0 0 (xcoord vel) (zcoord vel)]
                            ++ map (color red . drawAction place) (partsActions (Tick 1.0) craft))

drawCenter craft        = line [(-10, -15), (0, 15), (10,-15), (-10,-15)]

instance Drawable RigidPointObj where
    draw obj        = let place = objPlace obj
                      in pictures [line [(0,0), (double2Float (xcoord place), double2Float (zcoord place))], drawCircle place 3]

drawCircle          :: Place -> Double -> Picture
drawCircle place size = translate (double2Float (xcoord place)) (double2Float (zcoord place)) $ circle (double2Float size)

drawArrow           :: Double -> Double -> Double -> Double -> Picture
drawArrow x y xvel yvel = translate (double2Float x) (double2Float y) $ line [(0.0,0.0), (double2Float xvel, double2Float yvel)]

drawRectangle       :: Double -> Double -> Double -> Double -> Picture
drawRectangle cx cy width height = translate (double2Float cx) (double2Float cy) $ color (dark green) $ rectangleSolid (double2Float width) (double2Float height)

drawVector          :: Place -> Velocity -> Picture
drawVector place vel = drawArrow (xcoord place) (zcoord place) (xcoord vel) (zcoord vel)

