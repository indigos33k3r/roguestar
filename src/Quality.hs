module Quality
    (Quality(..),
     improve,
     reduce,
     qualitySOR,
     qualityFrame)
    where

import Model

data Quality = Bad
	     | Poor
	     | Good
	     | Super
	     deriving (Eq,Enum,Ord)
-- |
-- Might improve the Quality.  Appropriate for a major piece
-- of geometry that needs extra detail.
--
improve :: Quality -> Quality
improve Bad = Bad
improve Poor = Good
improve Good = Good
improve Super = Super

-- |
-- Might reduce the quality.  Appropriate for a small piece of
-- geometry that doesn't need much detail.
--
reduce :: Quality -> Quality
reduce Bad = Bad
reduce Poor = Poor
reduce Good = Poor
reduce Super = Good

-- |
-- SOR (as Model.sor) that introduces the number
-- of subdivisions automatically according to the quality setting.
--
qualitySOR :: Quality -> Material -> [Point2D] -> Model
qualitySOR Bad mat = sor mat 8
qualitySOR Poor mat = sor mat 16
qualitySOR Good mat = sor mat 22
qualitySOR Super mat = sor mat 28

-- |
-- Generates a frame (as Model.frame), running interpolation
-- over the points to enhance the model according to the quality.
--
qualityFrame :: Quality -> Material -> [[Point3D]] -> Model
qualityFrame Bad mat = frame mat
qualityFrame Poor mat = frame mat . enhancePoints
qualityFrame Good mat = frame mat . enhancePoints . enhancePoints
qualityFrame Super mat = frame mat . enhancePoints . enhancePoints . enhancePoints