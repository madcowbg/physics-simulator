{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
--
-- Module      :  Physics.Basic
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

module Physics.Basic (
    runBasicDemo
) where
-- for drawing
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort

--import Data.Vec --https://hackage.haskell.org/package/Vec-1.0.5/docs/Data-Vec.html
import Linear.V3

data Scene = Scene {crafts :: [Craft], forces :: [SceneForce]} --gravity :: Gravity, ground :: GroundForce}
data SceneForce = forall f. Force f => SceneForce {force :: f}

data Craft = Craft [Part] -- possibly, list of? accel :: Acceleration?

-- Time calculations
data Tick = Tick {s :: Float}

-- Particle State
data Part = Part {partState:: PartState}
data PartState = PartState {placeState :: PlaceState, rotationState :: RotationState}

-- States
type PlaceState = State Place Velocity Float
type RotationState = State Orientation Rotation (Rotation -> Float)
data State a da p = State {current :: a, flux :: da, property :: p}


-- Forces
class Force f where
    act                     :: f -> Tick -> PartState -> PartState
    act force tick (PartState placeState rotationState)
                            = PartState (updatePlaceState force tick placeState) (updateRotationState force tick rotationState)

    updatePlaceState        :: f -> Tick -> PlaceState -> PlaceState
    updateRotationState     :: f -> Tick -> RotationState -> RotationState

    getView                ::  f -> ((Place, Velocity) -> Picture) -> Picture

data Gravity = Gravity {direction :: Velocity, gconst :: Float}

instance Force Gravity where
   updatePlaceState force tick (State current flux prop)
                                    = State current (accellGravity force tick flux) prop
   updateRotationState f tick       = id
   getView (Gravity direction _) g  = g (V3 0 0 0, direction)

data GroundForce = GroundForce {zlim :: Float}
instance Force GroundForce where
    updateRotationState f tick      = id
    updatePlaceState (GroundForce zlim) tick state@(State current flux prop)
                                    = if zcoord current < zlim
                                        then State current (mirrorZ flux) prop
                                        else state
    getView _ g                     = g (V3 0 0 0, V3 0 0 0)

mirrorZ             :: Vector3 -> Vector3
mirrorZ (V3 x y z)  = V3 x y (-z)

accellGravity              :: Gravity -> Tick -> Velocity -> Velocity
accellGravity (Gravity direction gconst) (Tick s) flux = vectorMulAdd flux direction (s * gconst)

-- movement primitives
data Orientation    = Orientation {orient :: Vector3}
data Rotation       = Rotation {axis :: Velocity, speed :: Float}

type Place          = Vector3
type Velocity       = Vector3

type Vector3        = V3 Float


vectorMulAdd        :: Vector3 -> Vector3 -> Float -> Vector3
vectorMulAdd v1 v2 s = v1 + fmap (* s) v2


--instance (Change Orientation) Rotation where
--actOn            :: Acceleration
--actOn            = -- TODO math stuff


createScene :: Scene
createScene = let placeState = State (V3 0 0 100) (V3 3 0 15) 1
                  rotState   = State (Orientation (V3 0 0 1)) (Rotation (V3 0.1 0 0.1) 0) (const 1)
                  part = Part (PartState placeState rotState)
                  gravity = Gravity (V3 0 0 (-1)) 20
                  ground = GroundForce 0
              in Scene [Craft [part]] [SceneForce gravity, SceneForce ground]


class Evolving e where
    evolve          :: (PartState -> PartState) -> e -> e

instance Evolving Craft where
    evolve f (Craft parts)    = Craft (map (evolve f) parts)

instance Evolving Part where
    evolve f (Part state)     = Part (f state)


executeTick         :: Tick -> PartState -> PartState
executeTick tick (PartState placeState rotationState)
                    = PartState (moveState tick placeState) (rotateState tick rotationState)

executeForce         :: Tick -> SceneForce -> PartState -> PartState
executeForce tick (SceneForce force)
                    = act force tick

moveState           :: Tick -> PlaceState -> PlaceState
moveState (Tick s) (State place velocity mass)
                    = State (vectorMulAdd place velocity s) velocity mass

rotateState         :: Tick -> RotationState -> RotationState
rotateState (Tick s) -- (State orientation rotation angularMomentum)
                    = id -- TODO complex math here...

class Drawable d where
    draw        :: d -> Picture

instance Drawable Scene where
    draw (Scene crafts forces) = pictures (map draw crafts ++ map drawSceneForce forces)

drawSceneForce       :: SceneForce -> Picture
drawSceneForce (SceneForce force) = drawF force

drawF           :: (Force f) => f -> Picture
drawF g         =  getView g (\tuple -> drawVector (fst tuple) (snd tuple))

instance Drawable Craft where
    draw (Craft parts)   = pictures (map draw parts)

instance Drawable Part where
    draw (Part (PartState (State place vel _) _)) = pictures [
        drawCircle (xcoord place) (zcoord place) 20,
        drawArrow (xcoord place) (zcoord place) (xcoord vel) (zcoord vel)
        ]

instance Drawable Gravity where
    draw (Gravity direction gconst) = drawRectangle 0 (-15) 500 30

xcoord          :: Vector3 -> Float
xcoord (V3 x y z) = x

zcoord          :: Vector3 -> Float
zcoord (V3 x y z) = z

drawCircle          :: Float -> Float -> Float -> Picture
drawCircle x y size = translate x y $ circle 5

drawArrow           :: Float -> Float -> Float -> Float -> Picture
drawArrow x y xvel yvel = translate x y $ line [(0.0,0.0), (xvel,yvel)]

drawRectangle       :: Float -> Float -> Float -> Float -> Picture
drawRectangle cx cy width height = translate cx cy $ color (dark green) $ rectangleSolid width height

drawVector          :: Place -> Velocity -> Picture
drawVector place vel = drawArrow (xcoord place) (zcoord place) (xcoord vel) (zcoord vel)

window = InWindow "My Window" (500, 500) (0, 0)
fps = 30

update              :: ViewPort -> Float -> Scene -> Scene
update _ s (Scene crafts forces) = Scene evolvedCrafts forces
                                   where evolvedCrafts = map (evolve (compose (listOfEvolutions (Tick s) forces))) crafts

listOfEvolutions tick forces = executeTick tick:map (executeForce tick) forces

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

runBasicDemo    :: IO()
runBasicDemo    = simulate window white fps createScene draw update


