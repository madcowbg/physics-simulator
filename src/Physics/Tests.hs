-----------------------------------------------------------------------------
--
-- Module      :  Physics.Tests
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

module Physics.Tests (
    runBasicDemo
) where

import Physics.Primitives
import Physics.Objects
import Physics.BasicScene
import Physics.World
import Physics.Forces
import Physics.BasicDraw

-- for drawing
import Graphics.Gloss


createScene :: World
createScene = let placeState = PlaceState (makevect (-200) 0 100) (makevect 15 0 15)
                  rotState   = RotationState (makevect 0 0 1) (makevect 0.1 0 0.1) (const 1)
                  part = PhysicalObj (makevect 0 0 0) 1
                  gravity = Gravity (makevect 0 0 (-1)) 20
                  ground = GroundForce 0
              in World [Craft [part] placeState rotState] [GlobalForce gravity, GlobalForce ground] ground

window = InWindow "My Window" (500, 500) (0, 0)
fps = 30



runBasicDemo    :: IO()
runBasicDemo    = simulate window white fps createScene draw update

