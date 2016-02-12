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

--data Scene = Scene {crafts :: [Craft], forces :: [SceneForce]} --gravity :: Gravity, ground :: GroundForce}
--data SceneForce = forall f. Force f => SceneForce {force :: f} -- another way to incorporate forces -> split action and view

update      :: ViewPort -> Float -> World -> World
update _ t   = updateWorld (float2Double t)
