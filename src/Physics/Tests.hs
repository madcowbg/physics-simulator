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
import Physics.AbstractObjects
import Physics.Objects
import Physics.BasicScene
import Physics.World
import Physics.AbstractForces
import Physics.Forces
import Physics.BasicDraw

-- for drawing
import Graphics.Gloss


createScene :: SmallWorld
createScene = let gravity = Gravity (makevect 0 0 (-1)) 20
                  ground = BouncingGround 0
                  globalChain =  ForceChain gravity ForceEnd
                  drag  = AirResistance 0.4
                  placeState = PlaceState (makevect (-200) 0 100) (makevect 15 0 15)
                  rotState   = RotationState (makevect 0 0 1) (makevect 0.1 0 0.1)
                  part = RigidPointObj (makevect 0 0 0) 10 (ForceChain drag globalChain)
              in SmallWorld [RigidCraft [part] placeState rotState ground] ground

window = InWindow "My Window" (500, 500) (0, 0)
fps = 60



runBasicDemo    :: IO()
runBasicDemo    = simulate window white fps createScene draw update

