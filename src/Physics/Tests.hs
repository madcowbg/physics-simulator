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

import Physics.Coordinates
import Physics.Elementary
import Physics.Objects
import Physics.BasicScene
import Physics.World
import Physics.AbstractForces
import Physics.Forces
import Physics.BasicDraw
import Physics.Craft.Rigid
import Physics.Craft.Rocket

-- for drawing
import Graphics.Gloss


createScene :: SmallWorld
createScene = let gravity = Gravity (makevect 0 0 (-1)) 5--20
                  ground = BouncingGround (-200)
                  globalChain =  ForceChain gravity ForceEnd
                  dragLeft  = AirResistance 1--4
                  dragTop  = AirResistance 0--4
                  dragRight  = AirResistance 1-- .2
                  leftPart = RigidPointObj (makevect (-10) 0 (-15)) 5 (ForceChain dragLeft globalChain)
                  rightPart = RigidPointObj (makevect 10 0 (-15)) 10 (ForceChain dragRight globalChain)
                  topPart = RigidPointObj (makevect 0 0 15) 10 (ForceChain dragTop globalChain)
                  craft = createRigid [leftPart, rightPart, topPart] craftCoordinates ground
                  thruster = Thruster (makevect (0.01) 0 (50)) (ThrusterForce {maxPower = 30, percentThrust = 0.5, thrustDirection = makevect 0 0 (-20)})
                  rocket = Rocket craft [thruster]
                  craftCoordinates = CoordinateSystem GlobalSystem (makevect (200) 0 (-180)) (makevect 0 0 0) identityOrient (Rotation 0 0 0)
              in SmallWorld [rocket] ground

window = InWindow "My Window" (800, 800) (0, 0)
fps = 60



runBasicDemo    :: IO()
runBasicDemo    = simulate window white fps createScene draw update

