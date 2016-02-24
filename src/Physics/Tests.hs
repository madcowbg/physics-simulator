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
import Physics.Craft.Basic
import Physics.BasicScene
import Physics.World
import Physics.AbstractForces
import Physics.Forces
import Physics.BasicDraw
import Physics.Craft.Rigid
import Physics.Craft.Rocket
import Physics.Control.Basic

-- for drawing
import Graphics.Gloss


createScene :: SmallWorld
createScene = let gravity = Gravity (makevect 0 0 (-1)) 5--20
                  ground = BouncingGround (-200)
                  globalChain =  ForceChain gravity ForceEnd
                  dragLeft  = AirResistance 1--4
                  dragTop  = AirResistance 4
                  dragRight  = AirResistance 1-- .2
                  leftPart = RigidPointObj (makevect (-10) 0 (-15)) 5 (ForceChain dragLeft globalChain)
                  rightPart = RigidPointObj (makevect 10 0 (-15)) 10 (ForceChain dragRight globalChain)
                  topPart = RigidPointObj (makevect 0 0 15) 10 (ForceChain dragTop globalChain)
                  craft = createRigid [leftPart, rightPart, topPart] craftCoordinates ground
                  thruster = Thruster 0.5 (makevect (0.0) 0 (50)) ThrusterForce {maxPower = 30, thrustDirection = makevect 0 0 (-20)}
                  rocket = Rocket craft [thruster]
                  craftCoordinates = CoordinateSystem GlobalSystem (makevect (200) 0 (-180)) (makevect 0 0 0) identityOrient (Rotation 0 0 0)
                  firstStage = ControlState [Control 0.1]
                  secondStage = ControlState [Control 1]
                  thirdStage = ControlState [Control 0.15]
                  endStage = ControlState [Control 0]
                  controlSequence = [(2, firstStage), (6, secondStage), (10, thirdStage), (11, endStage)]
                  controlStrategy = ControlStrategy controlSequence
              in SmallWorld 0 [rocket] ground gravity controlStrategy

--data Control = Control {thrustLevel :: Double}
--data ControlStrategy = ControlStrategy {controlSequence :: [(Double, ControlState)] } | NoStrategy
--data ControlState = ControlState { thrustersState :: [Control]}

window = InWindow "My Window" (800, 800) (0, 0)
fps = 100



runBasicDemo    :: IO()
runBasicDemo    = simulate window white fps createScene draw update

