{-# LANGUAGE ScopedTypeVariables #-}

-- | All construction (terrain clearing, etc) actions that a creature might take.
module Roguestar.Lib.Behavior.Construction
    (modifyFacingTerrain,
     clearTerrain)
    where

import Roguestar.Lib.DB
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Data.PlaneData
import Roguestar.Lib.Data.TerrainData
import Roguestar.Lib.Data.FacingData
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Reader
import Roguestar.Lib.Position
import Data.Maybe

-- | Modifies terrain in the specified walking direction, returning
-- True iff any terrain modification actually occured.
modifyFacingTerrain :: (Terrain -> Terrain) -> Facing -> MonsterRef -> DB Bool
modifyFacingTerrain f face creature_ref = liftM (fromMaybe False) $ runMaybeT $
    do (Parent plane_ref :: Parent PlaneData,position :: Position) <- MaybeT $ liftM fromLocation $ asks $ whereIs creature_ref
       let target_position = offsetPosition (facingToRelative face) position
       prev_terrain <- lift $ terrainAt plane_ref target_position
       let new_terrain = f prev_terrain
       when (new_terrain == prev_terrain) $ fail ""
       lift $ setTerrainAt plane_ref target_position new_terrain
       return True

clearTerrain :: Terrain -> Terrain
clearTerrain RockFace = RockyGround
clearTerrain Forest = Grass
clearTerrain ForceField = RockyGround
clearTerrain x = x
