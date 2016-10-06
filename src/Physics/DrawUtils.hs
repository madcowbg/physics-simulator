-----------------------------------------------------------------------------
--
-- Module      :  Physics.DrawUtils
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

module Physics.DrawUtils (
    drawCircle,
    drawArrow,
    drawRectangle,
    drawVector,
    --
    translateD,
    printLog,
    showFixedHighPrecision,
    showFixed,
    ConsoleLog,
    ptPlaceCoord,
) where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Color

import GHC.Float
import Numeric

import Physics.Primitives
import Physics.Elementary

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


type ConsoleLog = [String]

printLog    :: Float -> Float -> Float -> Color -> ConsoleLog -> Picture
printLog xoff yoff textSize textColor lines
            = color textColor $ translate xoff yoff $ scale textSize textSize
                $ pictures (printall 0 lines)

printall        :: Float -> ConsoleLog -> [Picture]
printall offset [] = []
printall offset (line:lines) = translate 0 (-offset) (text line) : printall (offset + 150) lines

showFixed f = showFFloat (Just 2) f ""
showFixedHighPrecision f = showFFloat (Just 10) f ""

ptPlaceCoord place = (double2Float $ xcoord place, double2Float $ zcoord $ place)
