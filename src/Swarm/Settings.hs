-----------------------------------------------------------------------------
--
-- Module      :  Swarm.Settings
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

module Swarm.Settings (

    Preferences(Preferences),
    CoordBound(CoordBound),
    ArgSpace(ArgSpace),
    RndList(RndList)
) where

data (Num ct) => ArgSpace ct = ArgSpace {bounds :: [CoordBound ct]}
data (Num ct) => CoordBound ct = CoordBound {origin, scale :: ct}

data (Num ct) => Preferences ct = Preferences {
    inertia :: ct, -- how much a particle is guided by current direction
    cp :: ct, -- how much current local max is enticing
    cg :: ct, -- how much current global max is enticing
    vMax :: ct} -- max possible velocity


-- data and facilitis to execute random initializations
data (Num a) => RndList a        = RndList {rands :: [a], taken :: Integer}

