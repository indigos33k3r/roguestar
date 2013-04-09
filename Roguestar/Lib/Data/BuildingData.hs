
-- Data
module Roguestar.Lib.Data.BuildingData
    (Building(..),
     BuildingBehavior(..),
     BuildingShape(..),
     BuildingSignal(..),
     BuildingPrototype(..),
     basic_stargate,
     cybergate,
     monolith,
     powerup,
     buildingOccupies)
    where

import Roguestar.Lib.PowerUpData
import Roguestar.Lib.PersistantData

basic_stargate :: BuildingPrototype
basic_stargate = BuildingPrototype (TwoWayStargate NonAlignedRegion) Portal (Just Magnetic)

cybergate :: BuildingPrototype
cybergate = BuildingPrototype (OneWayStargate CyborgRegion) CyberGate (Just Magnetic)

monolith :: BuildingPrototype
monolith = BuildingPrototype (PowerUp $ ForceCharacter StarChild) Monolith (Just Magnetic)

powerup :: BuildingPrototype
powerup = BuildingPrototype (PowerUp $ AwardCharacter 1) Anchor (Just Magnetic)

-- | Get a list of squares, relative to the center of the building (0,0),
-- that a building occupies.  These squares must be free of unbuildable terrain
-- (mountains, trees, water, lava, etc.) and no other objects can co-occupy these squares.
--
-- It's also a hope that most buildings will be identifiable based on their footprint alone.
--
buildingOccupies :: BuildingShape -> [(Integer,Integer)]
-- Monolith/Node:  X
buildingOccupies Monolith = [(0,0)]
buildingOccupies Anchor   = [(0,0)]
-- Portal:
--
--    XXX
--  X     X
--  X  X  X
--  X     X
--    XXX
--
buildingOccupies Portal = [(0,0),(3,0),(3,1),(3,-1),(-3,0),(-3,1),(-3,-1),(0,3),(-1,3),(1,3),(0,-3),(-1,-3),(1,-3)]
--          X     X
buildingOccupies CyberGate = [(-3,-3),(-3,-2),(-2,-2),(-2,-1),(-1,-1),(-1,0),(0,0),(1,-1),(1,0),(2,-2),(2,-1),(3,-3),(3,-2)]

