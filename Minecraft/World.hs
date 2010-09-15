module Minecraft.World 
	( World (..)
	, newWorld
	, worldPlayerSpawn
	, addWorldPlayer
	, addWorldMob
	, addWorldItem
	, addWorldObject
	, removeWorldEntity
	, lookupWorldEntity
	, lookupWorldPlayerEntity
	, lookupWorldMobEntity
	, lookupWorldItemEntity
	, lookupWorldObjectEntity
	, updateWorldEntity
	, updateWorldPlayerEntity
	, updateWorldMobEntity
	, updateWorldItemEntity
	, updateWorldObjectEntity
	, moveWorldEntity
	) where
import Minecraft.Entity
import Minecraft.Entity.EntityId
import Minecraft.Entity.EntityPosition
import Minecraft.Player (Player)
import Minecraft.Mob (Mob)
import Minecraft.Item (Item)
import Minecraft.Object (Object)
import Minecraft.Map

import Data.Vector.Class
import Data.Vector.V3
import Control.Concurrent.MVar
import Data.Word
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe

type EntityMap v = IntMap v

data World = World
	{ worldAnyEntities :: MVar (EntityMap AnyEntity)
	, worldPlayerEntities :: MVar (EntityMap PlayerEntity)
	, worldMobEntities :: MVar (EntityMap MobEntity)
	, worldItemEntities :: MVar (EntityMap ItemEntity)
	, worldObjectEntities :: MVar (EntityMap ObjectEntity)
	, worldEntityIdGenerator :: MVar EntityIdGenerator
	, worldMap :: MinecraftMap
	}

moveWorldEntity :: Entity entity => World -> entity -> EntityPosition -> IO entity
moveWorldEntity world entity position = do
	let newEntity = setEntityPosition entity position
	updateWorldEntity world newEntity
	return newEntity

updateWorldPlayerEntity :: World -> PlayerEntity -> IO ()
updateWorldPlayerEntity
	World
		{ worldAnyEntities = mAnyEntities
		, worldPlayerEntities = mPlayerEntities
		}
	playerEntity@PlayerEntity
		{ playerEntityId = eid
		}
	= do
		anyEntities <- takeMVar mAnyEntities
		playerEntities <- takeMVar mPlayerEntities
		putMVar mPlayerEntities (IntMap.insert (fromIntegral eid) playerEntity playerEntities)
		putMVar mAnyEntities (IntMap.insert (fromIntegral eid) (AnyEntityPlayer playerEntity) anyEntities)

updateWorldMobEntity :: World -> MobEntity -> IO ()
updateWorldMobEntity
	World
		{ worldAnyEntities = mAnyEntities
		, worldMobEntities = mMobEntities
		}
	mobEntity@MobEntity
		{ mobEntityId = eid
		}
	= do
		anyEntities <- takeMVar mAnyEntities
		mobEntities <- takeMVar mMobEntities
		putMVar mMobEntities (IntMap.insert (fromIntegral eid) mobEntity mobEntities)
		putMVar mAnyEntities (IntMap.insert (fromIntegral eid) (AnyEntityMob mobEntity) anyEntities)

updateWorldItemEntity :: World -> ItemEntity -> IO ()
updateWorldItemEntity
	World
		{ worldAnyEntities = mAnyEntities
		, worldItemEntities = mItemEntities
		}
	itemEntity@ItemEntity
		{ itemEntityId = eid
		}
	= do
		anyEntities <- takeMVar mAnyEntities
		itemEntities <- takeMVar mItemEntities
		putMVar mItemEntities (IntMap.insert (fromIntegral eid) itemEntity itemEntities)
		putMVar mAnyEntities (IntMap.insert (fromIntegral eid) (AnyEntityItem itemEntity) anyEntities)


updateWorldObjectEntity :: World -> ObjectEntity -> IO ()
updateWorldObjectEntity
	World
		{ worldAnyEntities = mAnyEntities
		, worldObjectEntities = mObjectEntities
		}
	objectEntity@ObjectEntity
		{ objectEntityId = eid
		}
	= do
		anyEntities <- takeMVar mAnyEntities
		objectEntities <- takeMVar mObjectEntities
		putMVar mObjectEntities (IntMap.insert (fromIntegral eid) objectEntity objectEntities)
		putMVar mAnyEntities (IntMap.insert (fromIntegral eid) (AnyEntityObject objectEntity) anyEntities)


updateWorldEntity :: Entity entity => World -> entity -> IO ()
updateWorldEntity world entity = do
	case toAnyEntity entity of
		(AnyEntityPlayer e) -> updateWorldPlayerEntity world e
		(AnyEntityMob e) -> updateWorldMobEntity world e
		(AnyEntityItem e) -> updateWorldItemEntity world e
		(AnyEntityObject e) -> updateWorldObjectEntity world e

generateWorldEntityId :: World -> IO Word32
generateWorldEntityId World {worldEntityIdGenerator = mEntityIdGen} =
	modifyMVar mEntityIdGen (return . fromJust . generateEntityId)

addWorldPlayer :: World -> Player -> IO PlayerEntity
addWorldPlayer world@World
	{ worldAnyEntities = mAnyEntities
	, worldPlayerEntities = mPlayerEntities
	} player = do
		entityId <- generateWorldEntityId world
		spawn <- worldPlayerSpawn world player
		let playerEntity = PlayerEntity
			{ playerEntityId = entityId
			, playerEntityPosition = spawn
			, playerEntityPlayer = player
			}
		let anyEntity = toAnyEntity playerEntity
		anyEntities <- takeMVar mAnyEntities
		playerEntities <- takeMVar mPlayerEntities
		putMVar mAnyEntities (IntMap.insert (fromIntegral entityId) anyEntity anyEntities)
		putMVar mPlayerEntities (IntMap.insert (fromIntegral entityId) playerEntity playerEntities)
		return playerEntity

addWorldMob :: World -> Mob -> EntityPosition -> IO MobEntity
addWorldMob world@World
	{ worldAnyEntities = mAnyEntities
	, worldMobEntities = mMobEntities
	} mob pos = do
		entityId <- generateWorldEntityId world
		let mobEntity = MobEntity
			{ mobEntityId = entityId
			, mobEntityPosition = pos
			, mobEntityMob = mob
			}
		let anyEntity = toAnyEntity mobEntity
		anyEntities <- takeMVar mAnyEntities
		mobEntities <- takeMVar mMobEntities
		putMVar mAnyEntities (IntMap.insert (fromIntegral entityId) anyEntity anyEntities)
		putMVar mMobEntities (IntMap.insert (fromIntegral entityId) mobEntity mobEntities)
		return mobEntity

addWorldItem :: World -> Item -> EntityPosition -> IO ItemEntity
addWorldItem world@World
	{ worldAnyEntities = mAnyEntities
	, worldItemEntities = mItemEntities
	} item pos = do
		entityId <- generateWorldEntityId world
		let itemEntity = ItemEntity
			{ itemEntityId = entityId
			, itemEntityPosition = pos
			, itemEntityItem = item
			}
		let anyEntity = toAnyEntity itemEntity
		anyEntities <- takeMVar mAnyEntities
		itemEntities <- takeMVar mItemEntities
		putMVar mAnyEntities (IntMap.insert (fromIntegral entityId) anyEntity anyEntities)
		putMVar mItemEntities (IntMap.insert (fromIntegral entityId) itemEntity itemEntities)
		return itemEntity

addWorldObject :: World -> Object -> EntityPosition -> IO ObjectEntity
addWorldObject world@World
	{ worldAnyEntities = mAnyEntities
	, worldObjectEntities = mObjectEntities
	} object pos = do
		entityId <- generateWorldEntityId world
		let objectEntity = ObjectEntity
			{ objectEntityId = entityId
			, objectEntityPosition = pos
			, objectEntityObject = object
			}
		let anyEntity = toAnyEntity objectEntity
		anyEntities <- takeMVar mAnyEntities
		objectEntities <- takeMVar mObjectEntities
		putMVar mAnyEntities (IntMap.insert (fromIntegral entityId) anyEntity anyEntities)
		putMVar mObjectEntities (IntMap.insert (fromIntegral entityId) objectEntity objectEntities)
		return objectEntity

worldPlayerSpawn :: World -> Player -> IO EntityPosition
worldPlayerSpawn _ _ = return EntityPosition
	{ entityPositionVector = Vector3
		{ v3x = 10
		, v3y = 20
		, v3z = 10
		}
	, entityPositionRotation = 0
	, entityPositionPitch = 0
	}
{-
addWorldEntity :: Entity entity => World -> entity -> IO ()
addWorldEntity World
	{ worldAnyEntities = mAnyEntities
	, worldPlayerEntities = mPlayerEntities
	, worldMobEntities = mMobEntities
	, worldItemEntities = mItemEntities
	, worldObjectEntities = mObjectEntities
	} entity = do
	anyEntities <- takeMVar mAnyEntities
	let i = (entityId entity)
	let putAny = putMVar mAnyEntities (IntMap.insert (fromIntegral i) entity anyEntities)
	case toAnyEntity entity of
		(AnyEntityPlayer playerEntity) -> do
			playerEntities <- takeMVar mPlayerEntities
			putAny
			putMVar mPlayerEntities (IntMap.insert (fromIntegral i) playerEntity playerEntities)
		(AnyEntityMob mobEntity) -> do
			mobEntities <- takeMVar mMobEntities
			putAny
			putMVar mMobEntities (IntMap.insert (fromIntegral i) mobEntity mobEntities)
		(AnyEntityItem itemEntity) -> do
			itemEntities <- takeMVar mItemEntities
			putAny
			putMVar mItemEntities (IntMap.insert (fromIntegral i) itemEntity itemEntities)
		(AnyEntityObject objectEntity) -> do
			objectEntities <- takeMVar mObjectEntities
			putAny
			putMVar mObjectEntities (IntMap.insert (fromIntegral i) objectEntity objectEntities)
-}
removeWorldEntity :: Entity entity => World -> entity -> IO ()
removeWorldEntity World
	{ worldAnyEntities = mAnyEntities
	, worldPlayerEntities = mPlayerEntities
	, worldMobEntities = mMobEntities
	, worldItemEntities = mItemEntities
	, worldObjectEntities = mObjectEntities
	} entity = do
	anyEntities <- takeMVar mAnyEntities
	let i = (entityId entity)
	let putAny = putMVar mAnyEntities (IntMap.delete (fromIntegral i) anyEntities)
	case toAnyEntity entity of
		(AnyEntityPlayer _) -> do
			playerEntities <- takeMVar mPlayerEntities
			putAny
			putMVar mPlayerEntities (IntMap.delete (fromIntegral i) playerEntities)
		(AnyEntityMob _) -> do
			mobEntities <- takeMVar mMobEntities
			putAny
			putMVar mMobEntities (IntMap.delete (fromIntegral i) mobEntities)
		(AnyEntityItem _) -> do
			itemEntities <- takeMVar mItemEntities
			putAny
			putMVar mItemEntities (IntMap.delete (fromIntegral i) itemEntities)
		(AnyEntityObject _) -> do
			objectEntities <- takeMVar mObjectEntities
			putAny
			putMVar mObjectEntities (IntMap.delete (fromIntegral i) objectEntities)

lookupWorldEntity :: World -> Word32 -> IO (Maybe AnyEntity)
lookupWorldEntity World {worldAnyEntities = mAnyEntities} i = do
	anyEntities <- readMVar mAnyEntities
	return (IntMap.lookup (fromIntegral i) anyEntities)

lookupWorldPlayerEntity :: World -> Word32 -> IO (Maybe PlayerEntity)
lookupWorldPlayerEntity World {worldPlayerEntities = mPlayerEntities} i = do
	playerEntities <- readMVar mPlayerEntities
	return (IntMap.lookup (fromIntegral i) playerEntities)

lookupWorldMobEntity :: World -> Word32 -> IO (Maybe MobEntity)
lookupWorldMobEntity World {worldMobEntities = mMobEntities} i = do
	mobEntities <- readMVar mMobEntities
	return (IntMap.lookup (fromIntegral i) mobEntities)

lookupWorldItemEntity :: World -> Word32 -> IO (Maybe ItemEntity)
lookupWorldItemEntity World {worldItemEntities = mItemEntities} i = do
	itemEntities <- readMVar mItemEntities
	return (IntMap.lookup (fromIntegral i) itemEntities)

lookupWorldObjectEntity :: World -> Word32 -> IO (Maybe ObjectEntity)
lookupWorldObjectEntity World {worldObjectEntities = mObjectEntities} i = do
	objectEntities <- readMVar mObjectEntities
	return (IntMap.lookup (fromIntegral i) objectEntities)

newWorld :: IO World
newWorld = do
	entities <- newMVar IntMap.empty
	players <- newMVar IntMap.empty
	mobs <- newMVar IntMap.empty
	items <- newMVar IntMap.empty
	objects <- newMVar IntMap.empty
	minecraftMap <- newMinecraftMap
	idGen <- newMVar newEntityIdGenerator
	return World
		{ worldAnyEntities = entities
		, worldPlayerEntities = players
		, worldMobEntities = mobs
		, worldItemEntities = items
		, worldObjectEntities = objects
		, worldEntityIdGenerator = idGen
		, worldMap = minecraftMap
		}