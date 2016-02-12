-----------------------------------------------------------------------------
--
-- Module      :  Swarm.VectorArithm
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

module Swarm.VectorArithm (
    Vector,
    add, sub, scl, mul, cap
) where

import qualified Data.Vector as V
type Vector = V.Vector

add     :: (Num a) => Vector a -> Vector a -> Vector a
add     = V.zipWith (+)
sub     :: (Num a) => Vector a -> Vector a -> Vector a
sub     = V.zipWith (-)
scl     :: (Num a) => a -> Vector a -> Vector a
scl x   = V.map (* x)
mul     :: (Num a) => Vector a -> Vector a -> Vector a
mul     = V.zipWith (*)
cap     :: (Num a, Ord a) => a -> Vector a -> Vector a
cap x   = V.map (\y -> if y > x then x else if y < -x then -x else y )

