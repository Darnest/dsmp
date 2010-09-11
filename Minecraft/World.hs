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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type EntityMap v = Map Word32 v

data World = World
	{ worldAnyEntities :: MVar (EntityMap AnyEntity)
	, worldPlayerEntities :: MVar (EntityMap PlayerEntity)
	, worldMobEntities :: MVar (EntityMap MobEntity)
	, worldItemEntities :: MVar (EntityMap ItemEntity)
	, worldObjectEntities :: MVar (EntityMap ObjectEntity)
	, worldEntityIdGenerator :: MVar EntityIdGenerator
	, worldMap :: MinecraftMap
	}

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
		putMVar mAnyEntities (Map.insert entityId anyEntity anyEntities)
		putMVar mPlayerEntities (Map.insert entityId playerEntity playerEntities)
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
		putMVar mAnyEntities (Map.insert entityId anyEntity anyEntities)
		putMVar mMobEntities (Map.insert entityId mobEntity mobEntities)
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
		putMVar mAnyEntities (Map.insert entityId anyEntity anyEntities)
		putMVar mItemEntities (Map.insert entityId itemEntity itemEntities)
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
		putMVar mAnyEntities (Map.insert entityId anyEntity anyEntities)
		putMVar mObjectEntities (Map.insert entityId objectEntity objectEntities)
		return objectEntity

worldPlayerSpawn :: World -> Player -> IO EntityPosition
worldPlayerSpawn _ _ = return EntityPosition
	{ entityPositionVector = Vector3
		{ v3x = 0
		, v3y = 64
		, v3z = 0
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
	let putAny = putMVar mAnyEntities (Map.insert i entity anyEntities)
	case toAnyEntity entity of
		(AnyEntityPlayer playerEntity) -> do
			playerEntities <- takeMVar mPlayerEntities
			putAny
			putMVar mPlayerEntities (Map.insert i playerEntity playerEntities)
		(AnyEntityMob mobEntity) -> do
			mobEntities <- takeMVar mMobEntities
			putAny
			putMVar mMobEntities (Map.insert i mobEntity mobEntities)
		(AnyEntityItem itemEntity) -> do
			itemEntities <- takeMVar mItemEntities
			putAny
			putMVar mItemEntities (Map.insert i itemEntity itemEntities)
		(AnyEntityObject objectEntity) -> do
			objectEntities <- takeMVar mObjectEntities
			putAny
			putMVar mObjectEntities (Map.insert i objectEntity objectEntities)
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
	let putAny = putMVar mAnyEntities (Map.delete i anyEntities)
	case toAnyEntity entity of
		(AnyEntityPlayer _) -> do
			playerEntities <- takeMVar mPlayerEntities
			putAny
			putMVar mPlayerEntities (Map.delete i playerEntities)
		(AnyEntityMob _) -> do
			mobEntities <- takeMVar mMobEntities
			putAny
			putMVar mMobEntities (Map.delete i mobEntities)
		(AnyEntityItem _) -> do
			itemEntities <- takeMVar mItemEntities
			putAny
			putMVar mItemEntities (Map.delete i itemEntities)
		(AnyEntityObject _) -> do
			objectEntities <- takeMVar mObjectEntities
			putAny
			putMVar mObjectEntities (Map.delete i objectEntities)

lookupWorldEntity :: World -> Word32 -> IO (Maybe AnyEntity)
lookupWorldEntity World {worldAnyEntities = mAnyEntities} i = do
	anyEntities <- readMVar mAnyEntities
	return (Map.lookup i anyEntities)

lookupWorldPlayerEntity :: World -> Word32 -> IO (Maybe PlayerEntity)
lookupWorldPlayerEntity World {worldPlayerEntities = mPlayerEntities} i = do
	playerEntities <- readMVar mPlayerEntities
	return (Map.lookup i playerEntities)

lookupWorldMobEntity :: World -> Word32 -> IO (Maybe MobEntity)
lookupWorldMobEntity World {worldMobEntities = mMobEntities} i = do
	mobEntities <- readMVar mMobEntities
	return (Map.lookup i mobEntities)

lookupWorldItemEntity :: World -> Word32 -> IO (Maybe ItemEntity)
lookupWorldItemEntity World {worldItemEntities = mItemEntities} i = do
	itemEntities <- readMVar mItemEntities
	return (Map.lookup i itemEntities)

lookupWorldObjectEntity :: World -> Word32 -> IO (Maybe ObjectEntity)
lookupWorldObjectEntity World {worldObjectEntities = mObjectEntities} i = do
	objectEntities <- readMVar mObjectEntities
	return (Map.lookup i objectEntities)

newWorld :: IO World
newWorld = do
	entities <- newMVar Map.empty
	players <- newMVar Map.empty
	mobs <- newMVar Map.empty
	items <- newMVar Map.empty
	objects <- newMVar Map.empty
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