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

import Physics.World

import Graphics.Gloss.Data.ViewPort

import GHC.Float

update      :: (World w) => ViewPort -> Float -> w -> w
update _ t   = updateWorld (float2Double t)
