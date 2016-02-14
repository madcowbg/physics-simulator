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
import Physics.Craft.Rigid

-- for drawing
import Graphics.Gloss


createScene :: SmallWorld
createScene = let gravity = Gravity (makevect 0 0 (-1)) 5--20
                  ground = BouncingGround 0
                  globalChain =  ForceChain gravity ForceEnd
                  dragLeft  = AirResistance 4--4
                  dragTop  = AirResistance 0.2--4
                  dragRight  = AirResistance 0-- .2
                  placeState = PlaceState (makevect (-200) 0 200) (makevect 0 0 0) --(makevect 15 0 15)
                  rotState   = RotationState identityOrient (Rotation 0 0 0) --(Rotation 0 0.01 0)
                  leftPart = RigidPointObj (makevect (-10) 0 (-15)) 5 (ForceChain dragLeft globalChain)
                  rightPart = RigidPointObj (makevect 10 0 (-15)) 10 (ForceChain dragRight globalChain)
                  topPart = RigidPointObj (makevect 0 0 (15)) 10 (ForceChain dragTop globalChain)
              in SmallWorld [RigidCraft [leftPart, rightPart, topPart] placeState rotState ground] ground

window = InWindow "My Window" (500, 500) (0, 0)
fps = 60



runBasicDemo    :: IO()
runBasicDemo    = simulate window white fps createScene draw update

