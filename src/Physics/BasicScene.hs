-----------------------------------------------------------------------------
--
-- Module      :  Physics.BasicScene
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

module Physics.BasicScene (
    update
) where


import Physics.Primitives
import Physics.Objects
import Physics.Forces
import Physics.Time
import Physics.BasicDraw
import Physics.World

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort

import GHC.Float

update      :: (World w) => ViewPort -> Float -> w -> w
update _ t   = updateWorld (float2Double t/5)
