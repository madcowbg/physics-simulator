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
    mirrorZvel,
    mirrorZpos,
    stickZvel,

    xcoord,
    ycoord,
    zcoord,

    ------- ok?
    orientVector,
    reverseOrientVector,
    vectorScale,
    rotateOrientation,
    vectorSum,
    vectorMulAdd,
    vdot,
    vectorLength,
    vectorWithLength,
    --------
    crossAsMatrix,
    inertiaMatrixComponent,
    calculateAngularAcceleration,
    cross,
) where

import Physics.Elementary
import Linear.V3
import Linear.Matrix
import Linear.Vector

type Vector3        = V3 Double
type Matrix33       = M33 Double

vectorMulAdd        :: Vector3 -> Vector3 -> Double -> Vector3
vectorMulAdd v1 v2 s = v1 + vectorScale v2 s

vectorScale         :: Vector3 -> Double -> Vector3
vectorScale v1 s    = fmap (* s) v1

vectorSum           :: [Vector3] -> Vector3
vectorSum           = foldr (+) (makevect 0 0 0)

vectorLength        :: Vector3 -> Double
vectorLength vec    = sqrt (vdot vec vec)

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

rodriguesFormula        :: Vector3 -> RotationMatrix
rodriguesFormula v@(V3 x y z)   | theta == 0     = identityOrient
                                | otherwise      = cos theta *!! identityOrient
                                                    + (1-cos theta) *!! eeT e
                                                    + sin theta *!! crossAsMatrix e 
                                where theta = vectorLength v
                                      e = vectorScale v (1/theta)

eeT                 :: Vector3 -> Matrix33
eeT v@(V3 x y z)      = V3 (x*^v) (y*^v) (z*^v)

rotateOrientation       :: Orientation -> AngularVelocity -> Double -> Orientation
rotateOrientation orientation angularVelocity s
                        = rodriguesFormula (-s *^ angularVelocity) !*! orientation

orientVector       :: Orientation -> Vector3 -> Vector3
orientVector o v   = o !* v

reverseOrientVector       :: Orientation -> Vector3 -> Vector3
reverseOrientVector o v   = transpose o !* v

crossAsMatrix         :: Vector3 -> Matrix33
crossAsMatrix (V3 x y z)  = V3 (V3 0 (-z) y) (V3 z 0 (-x)) (V3 (-y) x 0)

inertiaMatrixComponent  :: (Place, Double) -> InertiaMatrix
inertiaMatrixComponent (place, mass)
                    = (-mass) *!! (vcross !*! vcross)
                        where vcross = crossAsMatrix place 

calculateAngularAcceleration
                    :: Torque -> InertiaMatrix -> AngularAcceleration
calculateAngularAcceleration torque inertiaMatrix
                    | torqueL == 0       = makevect 0 0 0
                    | otherwise          = torque ^* (1 / momInertia)
                    where torqueL = vectorLength torque
                          unitDirection = torque ^* (1/torqueL)
                          momInertia = vdot (unitDirection *! inertiaMatrix) unitDirection

vectorWithLength    :: Vector3 -> Double -> Vector3
vectorWithLength col size = col ^* (size / vectorLength col)

