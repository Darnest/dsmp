module Minecraft.Entity.EntityPosition
	( EntityPosition (..)
	, entityPositionMapBlockVector
	, entityPositionX
	, entityPositionY
	, entityPositionZ
	, entityPositionMapChunkVector
	) where
import Data.Vector.V3
import Minecraft.Map
import Data.Maybe

data EntityPosition
	= EntityPosition
		{ entityPositionVector :: Vector3
		, entityPositionRotation :: Double
		, entityPositionPitch :: Double
		}
	deriving (Eq, Show)

entityPositionMapBlockVector :: EntityPosition -> MapBlockVector
entityPositionMapBlockVector EntityPosition {entityPositionVector = positionVec} = 
	fromJust $ mapBlockVector (floor $ v3x positionVec) (floor $ v3y positionVec) (floor $ v3z positionVec)

entityPositionMapChunkVector :: EntityPosition -> MapChunkVector
entityPositionMapChunkVector = mapBlockVectorToChunkVector . entityPositionMapBlockVector

entityPositionX :: EntityPosition -> Double
entityPositionX = v3x . entityPositionVector

entityPositionY :: EntityPosition -> Double
entityPositionY = v3y . entityPositionVector

entityPositionZ :: EntityPosition -> Double
entityPositionZ = v3z . entityPositionVector