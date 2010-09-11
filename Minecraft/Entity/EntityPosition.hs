module Minecraft.Entity.EntityPosition
	( EntityPosition (..)
	, entityPositionMapBlockVector
	, entityPositionX
	, entityPositionY
	, entityPositionZ
	) where
import Data.Vector.V3
import Minecraft.Map

data EntityPosition
	= EntityPosition
		{ entityPositionVector :: Vector3
		, entityPositionRotation :: Double
		, entityPositionPitch :: Double
		}
	deriving (Eq, Show)

entityPositionMapBlockVector :: EntityPosition -> MapBlockVector
entityPositionMapBlockVector EntityPosition {entityPositionVector = positionVec} = 
	mapBlockVector (floor $ v3x positionVec) (floor $ v3y positionVec) (floor $ v3z positionVec)

entityPositionX :: EntityPosition -> Double
entityPositionX = v3x . entityPositionVector

entityPositionY :: EntityPosition -> Double
entityPositionY = v3y . entityPositionVector

entityPositionZ :: EntityPosition -> Double
entityPositionZ = v3z . entityPositionVector