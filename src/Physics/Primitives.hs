-----------------------------------------------------------------------------
--
-- Module      :  Physics.Primitives
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

module Physics.Primitives (
    Place,
    Velocity,
    Accelleration,
    ForceAmt,
    Orientation,
    Rotation (Rotation), Torque,
    MomentOfInertia,
    makevect,
    vectorMulAdd,
    vectorScale,
    vectorSum,
    xcoord,
    ycoord,
    zcoord,
    mirrorZvel,
    mirrorZpos,
    stickZvel,
    identityOrient,
    rotationOper,
    rotateOrientation,
    orientVector,
    reverseOrientVector,
    torqueSum, calcTorque, calculateRotationIntertia,
--    angleZdeg
    CoordinateSystem (GlobalSystem, CoordinateSystem),
    globalPlace, localPlace,
    globalState, localState
) where

import Linear.V3
import Linear.Matrix

type Vector3        = V3 Double
type Matrix33       = M33 Double


data CoordinateSystem = GlobalSystem | CoordinateSystem {parent :: CoordinateSystem, zeroLocation :: Place,
                                        velocity :: Velocity, orientation :: Orientation, rotation :: Rotation}

--instance Eq CoordinateSystem where
--    --(==)        :: CoordinateSystem -> CoordinateSystem -> Bool
--    (==) GlobalSystem GlobalSystem = True
--    (==) GlobalSystem _ = False
--    (==) _ GlobalSystem = False
--    (==) (CoordinateSystem { parent = p1 }) (CoordinateSystem { parent = p2 }) = p1 == p2


toParentPlace               :: CoordinateSystem -> Place -> Place
toParentPlace GlobalSystem _= error "cannot find parent of global system"
toParentPlace system place  = orientVector (orientation system) place + zeroLocation system

toChildPlace system place   = reverseOrientVector (orientation system) (zeroLocation system - place)

globalPlace                 :: CoordinateSystem -> Place -> Place
globalPlace                 = unpeel parent toParentPlace

localPlace                  :: CoordinateSystem -> Place -> Place
localPlace                  = peel parent toChildPlace

peel                        :: (CoordinateSystem -> CoordinateSystem) -> (CoordinateSystem -> a -> a) -> CoordinateSystem -> a -> a
peel f g GlobalSystem val   = val
peel f g system place       = g system (peel f g (f system) place)


unpeel                      :: (CoordinateSystem -> CoordinateSystem) -> (CoordinateSystem -> a -> a) -> CoordinateSystem -> a -> a
unpeel f g GlobalSystem val = val
unpeel f g system val       = unpeel f g (f system) (g system val)

toParentVelocity            :: CoordinateSystem -> Place -> Velocity -> Velocity
toParentVelocity GlobalSystem _ _ = error "cannot find parent of global system"
toParentVelocity system place vel
                            = velocity system + vel + calcRotationVelocity place (orientation system) (rotation system)

toChildVelocity             :: CoordinateSystem -> Place -> Velocity -> Velocity
toChildVelocity GlobalSystem _ _ = error "cannot find parent of global system"
toChildVelocity system place vel
                            = vel - velocity system - calcRotationVelocity place (orientation system) (rotation system)

globalState                 :: CoordinateSystem -> (Place, Velocity) -> (Place, Velocity)
globalState                 = unpeel parent (\system pv -> (toParentPlace system (fst pv), toParentVelocity system (fst pv) (snd pv)))

localState                  :: CoordinateSystem -> (Place, Velocity) -> (Place, Velocity)
localState                  = peel parent (\system pv -> (toChildPlace system (fst pv), toChildVelocity system (fst pv) (snd pv)))

-- TODO write in analytic form
deltaNumericalApprox = 0.001
calcRotationVelocity            :: Place -> Orientation -> Rotation -> Velocity
calcRotationVelocity place systemOrientation systemRotation
                                = vectorScale (orientVector deltaRotation place - orientVector systemOrientation place) (1/deltaNumericalApprox)
                                  where deltaRotation = rotateOrientation systemOrientation systemRotation deltaNumericalApprox



-- place & direction primitives
type Place          = Vector3
type Velocity       = Vector3

type Accelleration  = Vector3
type ForceAmt       = Vector3

type Orientation    = Matrix33

data Rotation       = Rotation {xEffect, yEffect, zEffect :: Double}
type MomentOfInertia= Rotation
type Torque         = Rotation

makevect            = V3

vectorMulAdd        :: Vector3 -> Vector3 -> Double -> Vector3
vectorMulAdd v1 v2 s = v1 + vectorScale v2 s

vectorScale         :: Vector3 -> Double -> Vector3
vectorScale v1 s    = fmap (* s) v1

vectorSum           :: [Vector3] -> Vector3
vectorSum           = foldr (+) (makevect 0 0 0)

vdot                :: Vector3 -> Vector3 -> Double
vdot (V3 x y z) (V3 sx sy sz) = x*sx + y*sy + z*sz

vectorSq            :: Vector3 -> Vector3
vectorSq (V3 x y z) = V3 (x*x) (y*y) (z*z)

xcoord          :: Vector3 -> Double
xcoord (V3 x y z) = x

ycoord          :: Vector3 -> Double
ycoord (V3 x y z) = y

zcoord          :: Vector3 -> Double
zcoord (V3 x y z) = z

mirrorZvel                  :: Velocity -> Velocity
mirrorZvel (V3 x y z)       = V3 x y (-z)

mirrorZpos                  :: Place -> Double -> Place
mirrorZpos (V3 px py pz) z  = V3 px py (z + (z-pz))


stickZvel                  :: Velocity -> Velocity
stickZvel (V3 x y z)       = V3 x y 0

identityOrient      :: Matrix33
identityOrient      = V3 (V3 1 0 0)
                         (V3 0 1 0)
                         (V3 0 0 1)

rotateX             :: Double -> Matrix33
rotateX angle       = V3 (V3 1          0                0)
                         (V3 0 (cos angle) (- (sin angle)))
                         (V3 0 (sin angle)     (cos angle))

rotateY             :: Double -> Matrix33
rotateY angle       = V3 (V3     (cos angle) 0  (sin angle))
                         (V3               0 1            0)
                         (V3 (- (sin angle)) 0  (cos angle))

rotateZ             :: Double -> Matrix33
rotateZ angle       = V3 (V3  (cos angle) (- (sin angle)) 0)
                         (V3  (sin angle)     (cos angle) 0)
                         (V3            0               0 1)

rotationOper            :: Rotation -> Double -> Orientation
rotationOper rotation s = rotateX (xEffect rotation) !*! rotateY (yEffect rotation) !*! rotateZ (zEffect rotation)


rotateOrientation       :: Orientation -> Rotation -> Double -> Orientation
rotateOrientation orientation rotation s
                        = rotationOper rotation s !*! orientation

orientVector       :: Orientation -> Vector3 -> Vector3
orientVector o v   = o !* v

reverseOrientVector       :: Orientation -> Vector3 -> Vector3
reverseOrientVector o v   = inv33 o !* v


--angleZdeg           :: Orientation -> Double
--angleZdeg orient    = asin (fv `vdot` ov) * 180 / pi
--                        where fv = V3 1 0 0
--                              ov = orientVector orient fv
-- --                     in  acos ((trace orient - 1) / 2) * 180 / pi

torqueSum       :: [Torque] -> Torque
torqueSum       = foldr plus (Rotation 0 0 0)

plus            :: Rotation -> Rotation -> Rotation
plus (Rotation x y z) (Rotation sx sy sz) = Rotation (x+sx) (y+sy) (z+sz)


calcTorque      :: Accelleration -> Place -> MomentOfInertia -> Torque
calcTorque forceAmt directionVector
                = asTorque (cross forceAmt directionVector)

asTorque        :: Vector3 -> MomentOfInertia -> Torque
asTorque (V3 x y z) (Rotation mx my mz)
                = Rotation (x/mx) (y/my) (z/mz)

calculateRotationIntertia :: Vector3 -> Double -> MomentOfInertia
calculateRotationIntertia (V3 x y z) mass
                = Rotation (mass * (y*y+z*z)) (mass*(x*x+z*z)) (mass*(x*x+y*y))

--toRotation :: Vector3 -> Rotation
--toRotation (V3 x y z) = Rotation (x+0.0001) (y+0.0001) (z+0.0001)
