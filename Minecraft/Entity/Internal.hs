{-# LANGUAGE RankNTypes, PatternGuards #-}
module Minecraft.Entity.Internal
	( Entity (..)
	, AnyEntity (..)
	, PlayerEntity (..)
	, MobEntity (..)
	, ItemEntity (..)
	, ObjectEntity (..)
	, EntityPosition (..)
	, entityPositionX
	, entityPositionY
	, entityPositionZ
	, entityX
	, entityY
	, entityZ
	, entityVector
	, entityRotation
	, entityPitch
	, entityBoundingBox
	, entityPositionMapBlockVector
	, entityPositionMapChunkVector
	, entityMapBlockVector
	, entityMapChunkVector
	) where
import Minecraft.Entity.EntityId
import Minecraft.Entity.EntityPosition
import Minecraft.Player (Player)
import Minecraft.Mob (Mob)
import Minecraft.Item (Item)
import Minecraft.Object (Object)
import Minecraft.Map


import Data.Vector.V3
import Data.Vector.Class
import Data.BoundingBox.B3
import Data.BoundingBox.Range
import Data.Word

class Entity entity where
	entityPosition :: entity -> EntityPosition
	entityId :: entity -> Word32
	entityBoundingBoxXDelta :: entity -> Double
	entityBoundingBoxYDelta :: entity -> Double
	entityBoundingBoxZDelta :: entity -> Double
	toAnyEntity :: entity -> AnyEntity
	setEntityPosition :: entity -> EntityPosition -> entity

entityVector :: Entity entity => entity -> Vector3
entityVector = entityPositionVector . entityPosition

entityX :: Entity entity => entity -> Double
entityX = entityPositionX . entityPosition

entityY :: Entity entity => entity -> Double
entityY = entityPositionY . entityPosition

entityZ :: Entity entity => entity -> Double
entityZ = entityPositionZ . entityPosition

entityRotation :: Entity entity => entity -> Double
entityRotation = entityPositionRotation . entityPosition

entityPitch :: Entity entity => entity -> Double
entityPitch = entityPositionPitch . entityPosition

entityMapBlockVector :: Entity entity => entity -> MapBlockVector
entityMapBlockVector = entityPositionMapBlockVector . entityPosition

entityMapChunkVector :: Entity entity => entity -> MapChunkVector
entityMapChunkVector = entityPositionMapChunkVector . entityPosition

entityBoundingBox :: Entity entity => entity -> BBox3
entityBoundingBox entity
	| Vector3
		{ v3x = x
		, v3y = y
		, v3z = z
		} <- entityVector entity
	= let
		xd = entityBoundingBoxXDelta entity
		yd = entityBoundingBoxYDelta entity
		zd = entityBoundingBoxZDelta entity
	in BBox3
		{ minX = x - xd
		, minY = y - yd
		, minZ = z - zd
		, maxX = x + xd
		, maxY = y + yd
		, maxZ = z + zd
		}

data AnyEntity
	= AnyEntityPlayer PlayerEntity
	| AnyEntityMob MobEntity
	| AnyEntityItem ItemEntity
	| AnyEntityObject ObjectEntity
	deriving (Eq)

fmapAnyEntity :: (forall entity. Entity entity => entity -> a) -> AnyEntity -> a
fmapAnyEntity f (AnyEntityPlayer entity) = f entity
fmapAnyEntity f (AnyEntityMob entity) = f entity
fmapAnyEntity f (AnyEntityItem entity) = f entity
fmapAnyEntity f (AnyEntityObject entity) = f entity

instance Entity AnyEntity where
	entityPosition = fmapAnyEntity entityPosition
	entityId = fmapAnyEntity entityId
	entityBoundingBoxXDelta = fmapAnyEntity entityBoundingBoxXDelta
	entityBoundingBoxYDelta = fmapAnyEntity entityBoundingBoxYDelta
	entityBoundingBoxZDelta = fmapAnyEntity entityBoundingBoxZDelta
	
	setEntityPosition (AnyEntityPlayer entity) = toAnyEntity . setEntityPosition entity
	setEntityPosition (AnyEntityMob entity) = toAnyEntity . setEntityPosition entity
	setEntityPosition (AnyEntityItem entity) = toAnyEntity . setEntityPosition entity
	setEntityPosition (AnyEntityObject entity) = toAnyEntity . setEntityPosition entity

	toAnyEntity = id

data PlayerEntity = PlayerEntity
	{ playerEntityId :: Word32
	, playerEntityPosition :: EntityPosition
	, playerEntityPlayer :: Player
	}
	deriving (Eq)

instance Entity PlayerEntity where
	entityPosition = playerEntityPosition
	entityId = playerEntityId
	entityBoundingBoxXDelta = undefined
	entityBoundingBoxYDelta = undefined
	entityBoundingBoxZDelta = undefined
	toAnyEntity = AnyEntityPlayer
	setEntityPosition playerEntity position = playerEntity {playerEntityPosition = position}


data MobEntity = MobEntity
	{ mobEntityId :: Word32
	, mobEntityPosition :: EntityPosition
	, mobEntityMob :: Mob
	}
	deriving (Show, Eq)

instance Entity MobEntity where
	entityPosition = mobEntityPosition
	entityId = mobEntityId
	entityBoundingBoxXDelta = undefined
	entityBoundingBoxYDelta = undefined
	entityBoundingBoxZDelta = undefined
	toAnyEntity = AnyEntityMob
	setEntityPosition mobEntity position = mobEntity {mobEntityPosition = position}


data ItemEntity = ItemEntity
	{ itemEntityId :: Word32
	, itemEntityPosition :: EntityPosition
	, itemEntityItem :: Item
	}
	deriving (Eq, Show)

instance Entity ItemEntity where
	entityPosition = itemEntityPosition
	entityId = itemEntityId
	entityBoundingBoxXDelta = undefined
	entityBoundingBoxYDelta = undefined
	entityBoundingBoxZDelta = undefined
	toAnyEntity = AnyEntityItem
	setEntityPosition itemEntity position = itemEntity {itemEntityPosition = position}


data ObjectEntity = ObjectEntity
	{ objectEntityId :: Word32
	, objectEntityPosition :: EntityPosition
	, objectEntityObject :: Object
	}
	deriving (Show, Eq)

instance Entity ObjectEntity where
	entityPosition = objectEntityPosition
	entityId = objectEntityId
	entityBoundingBoxXDelta = undefined
	entityBoundingBoxYDelta = undefined
	entityBoundingBoxZDelta = undefined
	toAnyEntity = AnyEntityObject
	setEntityPosition objectEntity position = objectEntity {objectEntityPosition = position}
