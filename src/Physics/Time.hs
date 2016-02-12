-----------------------------------------------------------------------------
--
-- Module      :  Physics.Time
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

module Physics.Time (
    --evolveCrafts,
    updateWorld,
    getActions
) where

import Physics.Primitives
import Physics.Objects
import Physics.Forces
import Physics.World


updateWorld              :: Double -> World -> World
updateWorld time  = let t = Tick time in runTime t . runForces t

runTime              :: Tick -> World -> World
runTime t (World crafts forces ground)
                    = World (map (executeTick t) crafts) forces ground

runForces           :: Tick -> World -> World
runForces t (World crafts forces ground)
                    = World (evolveCrafts t forces crafts) forces ground


executeTick         :: Tick -> Craft -> Craft
executeTick tick (Craft parts placeState rotationState)
                    = Craft parts (updatePlace tick placeState) rotationState

updatePlace    (Tick s) (PlaceState place velocity)
                    = PlaceState (vectorMulAdd place velocity s) velocity

evolveCrafts                :: Tick -> [GlobalForce] -> [Craft] -> [Craft]
evolveCrafts tick forces    = map (\craft -> applyActions (getActions tick craft forces) craft)

getActions                  ::  Tick-> Craft -> [GlobalForce] -> [ForceAction]
getActions tick craft     = concatMap (getAction craft tick)     -- TODO add internal forces...

getAction                   ::  Craft -> Tick -> GlobalForce -> [ForceAction]
getAction (Craft parts (PlaceState place _) _) tick GlobalForce {force = f}
                            = map (act f tick place) parts

applyActions                :: [ForceAction] -> Craft -> Craft
applyActions actions        = applyAction (mergeActions actions)

mergeActions                :: [ForceAction] -> (Velocity->Velocity, ForceAction)
mergeActions []             = (id, ForceAction (makevect 0 0 0) 0)
mergeActions (NoAction:as)  = mergeActions as
mergeActions (ShockAction place fun:as)
                            = let rest = mergeActions as in (fun . fst rest, snd rest)
mergeActions (a@(ForceAction place fun):as)
                            = let rest = mergeActions as in (fst rest, combine a (snd rest))

combine             :: ForceAction -> ForceAction -> ForceAction
combine (ForceAction place1 amt1) (ForceAction place2 amt2)
            = ForceAction (makevect 0 0 0) (amt1 + amt2)        -- FIXME calculate rotation as well...

applyAction         :: (Velocity->Velocity, ForceAction) -> Craft -> Craft
applyAction (shock, ForceAction place forceAmt) craft@(Craft parts (PlaceState craftPlace velocity) rotationState)
                    = Craft parts (PlaceState craftPlace (applyShocks velocity shock forceAmt (craftMass craft))) rotationState

applyShocks             :: Velocity -> (Velocity->Velocity) -> ForceAmt -> Double -> Velocity
applyShocks velocity shock forceAmt mass = shock (velocity + vectorScale forceAmt (1 / mass))

--(Craft parts placeState rotationState)


 --`                   Craft parts (applyAction actions

 -- Craft{parts :: [PhysicalObj], placeState :: PlaceState, rotationState :: RotationState}
--(ForceAction, ShockAction, NoAction)

--- THIS NEEDS SIGNIFICANT REWRITE

--class Evolving e where
--    evolve          :: (PhysicalObj -> PhysicalObj) -> e -> e
--
--instance Evolving Craft where
--    evolve f (Craft parts)    = Craft (map (evolve f) parts)
--
--instance Evolving PhysicalObj where
--    evolve f (PhysicalObj state)     = PhysicalObj (f state)
--
--
--class EvolvingState s where
--    executeTick     :: Tick -> s -> s
--
--instance EvolvingState PlaceState where
--    executeTick    (Tick s) (PlaceState place velocity mass)
--                    = PlaceState (vectorMulAdd place velocity s) velocity mass
--
--instance EvolvingState RotationState where
--    executeTick (Tick s) -- (RotationState orientation rotation angularMomentum)
--                    = id -- TODO complex math here...


--executeForce         :: Tick -> SceneForce -> PhysicalObj -> PhysicalObj
--executeForce tick (SceneForce force)
--                    = act force tick

--
--listOfEvolutions                :: Tick -> [Force f] -> Craft -> Craft
--listOfEvolutions tick forces     = executeTick tick:map (executeForce tick) forces
--
--compose :: [a -> a] -> a -> a
--compose = foldl (flip (.)) id
