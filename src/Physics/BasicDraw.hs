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
import Physics.Forces
import Physics.Time
import Physics.World

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import GHC.Float

class Drawable d where
    draw        :: d -> Picture


--class DrawableForce f where
--    drawForce     :: f -> PhysicalObj -> Picture
--
--instance DrawableForce Gravity where
--    drawForce (Gravity direction gconst) (PhysicalObj (PlaceState place _ _) _)
--                = drawVector place (direction * gconst)
--
--instance DrawableForce GroundForce where
--    drawForce (GroundForce zlim) (PhysicalObj (PlaceState place _)
--        = drawRectangle 0 (-15) 500 30


--    getView                ::  f -> ((Place, Velocity) -> Picture) -> Picture -- NOT GOOD
--
--type DrawObj f =
--
--
--
--    applicationPoint         :: f ->
--
--    gravity
--       getView (Gravity direction _) g  = g (Acceleration 0 0 0, direction) -- NOT GOOD
--
--       ground
--           getView _ g                     = g (Acceleration 0 0 0, Acceleration 0 0 0) -- NOT GOOD

--drawF           :: (Force f) => f -> Picture
--drawF g         =  getView g (\tuple -> drawVector (fst tuple) (snd tuple))


instance Drawable World where
    draw (World crafts forces ground) = pictures (map (color blue . draw) crafts ++ [drawForce ground] ++ map (color red . drawAction) (concatMap (\craft -> getActions (Tick 1.0) craft forces) crafts))
    --getAction                   ::  Craft -> Tick -> GlobalForce -> [ForceAction]
-- getActions                  ::  Tick-> Craft -> [GlobalForce] -> [ForceAction]

drawAction                              :: ForceAction -> Picture
drawAction NoAction                     = Blank
drawAction (ForceAction place forceAmt) = drawVector place forceAmt
drawAction (ShockAction place _)        = drawCircle place 0.2

class DrawableForce f where
    drawForce       :: f -> Picture

instance DrawableForce GroundForce where
    drawForce (GroundForce zlim)
                    = drawRectangle 0 (zlim-15) 1200 30


instance Drawable Craft where
    draw (Craft parts (PlaceState place vel) rotationState)
            = translate (double2Float (xcoord place)) (double2Float (zcoord place)) $ pictures (
            map draw parts ++ [drawArrow 0 0 (xcoord vel) (zcoord vel)])

instance Drawable PhysicalObj where -- NOT GOOD
    draw (PhysicalObj place vel) = pictures [drawCircle place 5]



drawCircle          :: Place -> Double -> Picture
drawCircle place size = translate (double2Float (xcoord place)) (double2Float (zcoord place)) $ circle (double2Float size)

drawArrow           :: Double -> Double -> Double -> Double -> Picture
drawArrow x y xvel yvel = translate (double2Float x) (double2Float y) $ line [(0.0,0.0), (double2Float xvel, double2Float yvel)]

drawRectangle       :: Double -> Double -> Double -> Double -> Picture
drawRectangle cx cy width height = translate (double2Float cx) (double2Float cy) $ color (dark green) $ rectangleSolid (double2Float width) (double2Float height)

drawVector          :: Place -> Velocity -> Picture
drawVector place vel = drawArrow (xcoord place) (zcoord place) (xcoord vel) (zcoord vel)

