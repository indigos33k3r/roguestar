{-# LANGUAGE OverloadedStrings #-}
module Roguestar.Lib.Core.Tests
    (testcases)
    where

import Control.Monad.Random
import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Data.TerrainData
import Roguestar.Lib.Data.ToolData
import Roguestar.Lib.DB
import Roguestar.Lib.Core.Plane
import Test.HUnit

testcases :: Test
testcases = TestLabel "Roguestar.Lib.Core.Tests" $ TestList [testAncestors]

spock :: MonsterData
spock = empty_monster

setupCreatureWithTool :: DB (ToolRef,MonsterRef,PlaneRef)
setupCreatureWithTool =
    do seed <- getRandom
       plane_ref <- dbNewPlane "vulcan" (TerrainGenerationData {
            tg_smootheness = 3,
            tg_biome = weightedSet [(1,CraterInterior)],
            tg_placements = [recreantFactories seed] }) TheUniverse
       monster_ref <- dbAddMonster spock $ Standing plane_ref (Position (0,0)) Here
       tool_ref <- dbAddTool phaser $ Inventory monster_ref
       return (tool_ref,monster_ref,plane_ref)

testAncestors :: Test
testAncestors = TestCase $
    do (Right ((tool_ref,creature_ref,plane_ref),setup_db)) <- runDB setupCreatureWithTool initial_db
       let ancestors = map parentReference $ getAncestors tool_ref setup_db
       assertEqual "testAncestors" [genericReference creature_ref,genericReference plane_ref,genericReference the_universe] ancestors

