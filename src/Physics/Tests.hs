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

import Physics.Coordinates.Rotating
import Physics.Elementary
import Physics.Craft.Basic
import Physics.World.Basic
import Physics.World.Small
import Physics.AbstractForces
import Physics.Forces
import Physics.BasicDraw
import Physics.Craft.Rigid
import Physics.Craft.Rocket
import Physics.Control.Basic
import Physics.Control.Criteria

-- for drawing
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import GHC.Float

-- for randoms
import System.Random
import Data.Random.Normal

import Physics.Planning.Linear

gravity = Gravity (makevect 0 0 (-1)) 5--20
ground = BouncingGround (-200)
globalChain =  ForceChain gravity ForceEnd
dragLeft  = AirResistance 0--4
dragTop  = AirResistance 0--2--4
dragRight  = AirResistance 0-- .2
leftPart = RigidPointObj (makevect (-10) 0 (-15)) 5 (ForceChain dragLeft globalChain)
rightPart = RigidPointObj (makevect 10 0 (-15)) 10 (ForceChain dragRight globalChain)
topPart = RigidPointObj (makevect 0 0 15) 10 (ForceChain dragTop globalChain)

craftZeroCoordinates = RotatingCoordinates GlobalSystem (InertialCoordinates (makevect (200) 0 (100)) (makevect (-20) 0 20) identityOrient) (makevect 0 0 0)
craft = createRigid [leftPart, rightPart, topPart] craftZeroCoordinates ground

symmRocket  :: Rocket
symmRocket  = let
                  topSymm = Thruster 0.5 (makevect 0 0 0) ThrusterForce {maxPower = 10, thrustDirection = makevect 0 0 (20)}
                  bottomSymm = Thruster 0.5 (makevect 0 0 0) ThrusterForce {maxPower = 10, thrustDirection = makevect 0 0 (-20)}
                  leftSymm = Thruster 0.5 (makevect 0 0 0) ThrusterForce {maxPower = 50, thrustDirection = makevect (-1) 0 0}
                  rightSymm = Thruster 0.5 (makevect 0 0 0) ThrusterForce {maxPower = 50, thrustDirection = makevect 1 0 0}
              in Rocket craft [topSymm, bottomSymm, leftSymm, rightSymm]

asymmRocket :: Rocket
asymmRocket = let
                  leftThruster = Thruster 0.5 (makevect (0.01) 0 (-10)) ThrusterForce {maxPower = 10, thrustDirection = makevect 0 0 (20)}
                  rightThruster = Thruster 0.5 (makevect (-0.01) 0 (-10)) ThrusterForce {maxPower = 10, thrustDirection = makevect 0 0 (-20)}
                  leftRCS = Thruster 0.5 (makevect (0) 0 (50)) ThrusterForce {maxPower = 5, thrustDirection = makevect (-1) 0 0}
                  rightRCS = Thruster 0.5 (makevect (0) 0 (50)) ThrusterForce {maxPower = 5, thrustDirection = makevect 1 0 0}
              in Rocket craft [leftThruster, rightThruster, leftRCS, rightRCS]

simpRocket :: Rocket
simpRocket  = let
                  leftThruster = Thruster 0.5 (makevect (0.01) 0 (-10)) ThrusterForce {maxPower = 10, thrustDirection = makevect 0 0 (20)}
                  rightThruster = Thruster 0.5 (makevect (-0.01) 0 (-10)) ThrusterForce {maxPower = 10, thrustDirection = makevect 0 0 (-20)}
              in Rocket craft [leftThruster, rightThruster]

passiveRocket :: Rocket
passiveRocket  = Rocket craft []

createScene :: SmallWorld
createScene
            = let
                  rocket = passiveRocket
--                  firstStage = ControlState [Control 0.1]
--                  secondStage = ControlState [Control 1]
--                  thirdStage = ControlState [Control 0.15]
--                  endStage = ControlState [Control 0]
--                  controlSequence = [(2, firstStage), (6, secondStage), (10, thirdStage), (11, endStage)]
--                  controlStrategy = ControlStrategy controlSequence
--                  world = SmallWorld 0 [rocket] ground gravity controlStrategy
                  world = SmallWorld 0 [rocket] ground gravity NoStrategy

                  --Double -> Criterion -> SmallWorld -> ControlStrategy
              in findBetterControl world


findBetterControl world = world {craftControl = findBestControls 1 criterion world}

criterion   = Criterion (makevect (200) 0 (-0)) atrest (CriterionW 1 0)

window = InWindow "My Window" (800, 800) (0, 0)
fps = 60

update      :: (World w) => ViewPort -> Float -> w -> w
update _ t   = updateWorld (float2Double t)

controlledUpdate :: Double -> ViewPort -> Float -> SmallWorld -> SmallWorld
controlledUpdate ups vp t world@(SmallWorld time _ _ _ _)
                    | fromInteger (ceiling (ups*time)) < ups*(time + float2Double t)
                                                    = update vp t (findBetterControl world)
                    | otherwise                     = update vp t world

runBasicDemo    :: IO()
runBasicDemo    = do
                    simulate window white fps (createScene) draw (controlledUpdate 0.02)

