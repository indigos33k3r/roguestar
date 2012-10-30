{-# LANGUAGE ScopedTypeVariables #-}

-- | All construction (terrain clearing, etc) actions that a creature might take.
module Roguestar.Lib.Behavior.Construction
    (modifyFacingTerrain,
     clearTerrain)
    where

import Roguestar.Lib.DB
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.PlaneData
import Roguestar.Lib.TerrainData
import Roguestar.Lib.Facing
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Trans
import Roguestar.Lib.Position
import Data.Maybe

-- | Modifies terrain in the specified walking direction, returning
-- True iff any terrain modification actually occured.
modifyFacingTerrain :: (Terrain -> Terrain) -> Facing -> CreatureRef -> DB Bool
modifyFacingTerrain f face creature_ref = liftM (fromMaybe False) $ runMaybeT $
    do (Parent plane_ref :: Parent Plane,position :: Position) <- MaybeT $ liftM fromLocation $ whereIs creature_ref
       let target_position = offsetPosition (facingToRelative face) position
       prev_terrain <- lift $ terrainAt plane_ref target_position
       let new_terrain = f prev_terrain
       when (new_terrain == prev_terrain) $ fail ""
       lift $ setTerrainAt plane_ref target_position new_terrain
       return True

clearTerrain :: Terrain -> Terrain
clearTerrain RockFace = RockyGround
clearTerrain Forest = Grass
clearTerrain x = x
