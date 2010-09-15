module Minecraft.Network.Protocol.ServerPacket
	( ServerPacket (..)
	) where
import Minecraft.Network.Protocol.Packet
import Minecraft.Network.Protocol.Data
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import qualified Data.Binary.IEEE754 as BSF
import qualified Codec.Compression.Zlib as Zlib
import Data.Int
import Data.Word
import Data.Bits
import Debug.Trace
import Control.Monad

data ServerPacket
	= KeepAlive
	| Login
		{ loginPlayerId :: Word32
		, loginServerName :: String
		, loginMOTD :: String
		}
	| Handshake
		{ handshakeServerId :: String
		}
	| Chat
		{ chatMessage :: String
		}
	| Time
		{ timeMinutes :: Int64
		}
	| SetPlayerInventory
		{ setPlayerInventoryType :: PlayerInventoryType
		, setPlayerInventoryCount :: Word16
		, setPlayerInventoryItems :: [Maybe (Word16, Word8, Word16)]
		}
	| PlayerOnGround
		{ playerOnGround :: Bool
		}
	| PlayerSpawn
		{ playerSpawnX :: Int32
		, playerSpawnY :: Int32
		, playerSpawnZ :: Int32
		}
	| PlayerPosition
		{ playerPositionX :: Double
		, playerPositionY :: Double
		, playerPositionStance :: Double
		, playerPositionZ :: Double
		, playerPositionOnGround :: Bool
		}
	| PlayerLook
		{ playerLookRotation :: Float
		, playerLookPitch :: Float
		, playerLookOnGround :: Bool
		}
	| PlayerPositionLook
		{ playerPositionLookX :: Double
		, playerPositionLookY :: Double
		, playerPositionLookStance :: Double
		, playerPositionLookZ :: Double
		, playerPositionLookRotation :: Float
		, playerPositionLookPitch :: Float
		, playerPositionLookOnGround :: Bool
		}
	| ArmMovement
		{ armMovementEntityId :: Word32
		, armMovementUnknown1 :: Int8
		}
	| NamedEntitySpawn
		{ namedEntitySpawnEntityId :: Word32
		, namedEntitySpawnName :: String
		, namedEntitySpawnX :: Int32
		, namedEntitySpawnY :: Int32
		, namedEntitySpawnZ :: Int32
		, namedEntitySpawnRotation :: Int8
		, namedEntitySpawnPitch :: Int8
		, namedEntitySpawnCurrentItem :: Int16
		}
	| ItemSpawn
		{ itemSpawnEntityId :: Word32
		, itemSpawnItemType :: Word16
		, itemSpawnUnknown1 :: Word8
		, itemSpawnX :: Int32
		, itemSpawnY :: Int32
		, itemSpawnZ :: Int32
		, itemSpawnRotation :: Int8
		, itemSpawnPitch :: Int8
		, itemSpawnUnknown2 :: Int8
		}
	| CollectItem
		{ collectItemItemEntityId :: Word32
		, collectItemCollectorEntityId :: Word32
		}
	| ObjectSpawn
		{ objectSpawnEntityId :: Word32
		, objectSpawnObjectType :: Word8
		, objectSpawnX :: Int32
		, objectSpawnY :: Int32
		, objectSpawnZ :: Int32
		}
	| HoldItem
		{ entityId :: Word32
		, itemType :: Word16
		}
	| AddInventoryItem
		{ pickupItemType :: Word16
		, pickupItemAmount :: Word8
		, pickupItemLife :: Word16
		}
	| MobSpawn
		{ mobSpawnEntityId :: Word32
		, mobSpawnMobType :: Word8
		, mobSpawnX :: Int32
		, mobSpawnY :: Int32
		, mobSpawnZ :: Int32
		, mobSpawnRotation :: Int8
		, mobSpawnPitch :: Int8
		}
	| DestroyEntity
		{ destroyEntityId :: Word32
		}
	| InitEnity
		{ initEntityId :: Word32
		}
	| EntityMove
		{ entityMoveEntityId :: Word32
		, entityMoveX :: Int8 -- / 32
		, entityMoveY :: Int8 -- / 32
		, entityMoveZ :: Int8 -- / 32
		}
	| EntityLook
		{ entityLookEntityId :: Word32
		, entityLookRotation :: Int8
		, entityLookPitch :: Int8
		}
	| EntityMoveLook
		{ entityMoveLookEntityId :: Word32
		, entityMoveLookX :: Int8 -- / 32
		, entityMoveLookY :: Int8 -- / 32
		, entityMoveLookZ :: Int8 -- / 32
		, entityMoveLookRotation :: Int8
		, entityMoveLookPitch :: Int8
		}
	| EntityPositionLook
		{ entityPositionLookEntityId :: Word32
		, entityPositionLookX :: Int32
		, entityPositionLookY :: Int32
		, entityPositionLookZ :: Int32
		, entityPositionLookRotation :: Int8
		, entityPositionLookPitch :: Int8
		}
	| PreChunk
		{ preChunkX :: Int32 -- x / 16
		, preChunkZ :: Int32 -- z / 16
		, preChunkMode :: Bool -- loading or simply deleting
		}
	| MapChunk
		{ mapChunkX :: Int32
		, mapChunkY :: Int16
		, mapChunkZ :: Int32
		, mapChunkXSize :: Word8 -- - 1
		, mapChunkYSize :: Word8 -- - 1
		, mapChunkZSize :: Word8 -- - 1
		, mapChunkBlockTypes :: [Word8]
		, mapChunkBlockMetadata :: [Word8]
		, mapChunkBlockLighting :: [Word8] -- unpacked
		}
	| MultiBlockChange
		{ multiBlockChangeX :: Int32 -- / 16
		, multiBlockChangeZ :: Int32 -- / 16
		, multiBlockChangeSize :: Int16
		, multiBlockChangeCoordinates :: [(Word8, Word8, Word8)] -- (4 bits x, 4 bits z, 8 bits y) in protocol
		, multiBlockChangeBlockTypes :: [Word8]
		, multiBlockChangeMetadata :: [Word8]
		}
	| BlockChange
		{ blockChangeX :: Int32
		, blockChangeY :: Int8
		, blockChangeZ :: Int32
		, blockChangeBlockType :: Word8
		, blockChangeBlockMetadata :: Word8
		}
	| Kick
		{ kickMessage :: String
		}
	deriving (Show, Eq)

instance Packet ServerPacket where
	packetId KeepAlive {}          = 0x00
	packetId Login {}              = 0x01
	packetId Handshake {}          = 0x02
	packetId Chat {}               = 0x03
	packetId Time {}               = 0x04
	packetId SetPlayerInventory {} = 0x05
	packetId PlayerSpawn {}        = 0x06
	packetId PlayerOnGround {}     = 0x0A
	packetId PlayerPosition {}     = 0x0B
	packetId PlayerLook {}         = 0x0C
	packetId PlayerPositionLook {} = 0x0D
	packetId HoldItem {}           = 0x10
	packetId AddInventoryItem {}   = 0x11
	packetId ArmMovement {}        = 0x12
	packetId NamedEntitySpawn {}   = 0x14
	packetId ItemSpawn {}          = 0x15
	packetId CollectItem {}        = 0x16
	packetId ObjectSpawn {}        = 0x17
	packetId MobSpawn {}           = 0x18
	packetId DestroyEntity {}      = 0x1D
	packetId InitEnity {}          = 0x1E
	packetId EntityMove {}         = 0x1F
	packetId EntityLook {}         = 0x20
	packetId EntityMoveLook {}     = 0x21
	packetId EntityPositionLook {} = 0x22
	packetId PreChunk {}           = 0x32
	packetId MapChunk {}           = 0x33
	packetId MultiBlockChange {}   = 0x34
	packetId BlockChange {}        = 0x35
	packetId Kick {}               = 0xFF
	
	getPacketContents 0x01 = do
		playerId <- Get.getWord32be
		serverName <- getPacketString
		motd <- getPacketString
		return (Login playerId serverName motd)
	
	getPacketContents 0x02 = do
		serverId <- getPacketString
		return (Handshake serverId)
	
	getPacketContents _ = undefined
	
	putPacketContents KeepAlive = return ()
	
	putPacketContents (Login playerId serverName motd) = do
		Put.putWord32be playerId
		putPacketString serverName
		putPacketString motd
		
	putPacketContents (Handshake serverId) = do
		putPacketString serverId
		
	putPacketContents (Chat message) = do
		putPacketString message
		
	putPacketContents (Time minutes) = do
		Put.putWord64be (fromIntegral minutes)
	
	putPacketContents (SetPlayerInventory inventoryType count inventoryData) = do
		putPlayerInventoryType inventoryType
		Put.putWord16be count
		mapM_ (\itemData ->
			case itemData of
				(Just (itemId, itemCount, itemHealth)) -> do
					Put.putWord16be itemId
					Put.putWord8 itemCount
					Put.putWord16be itemHealth
				Nothing -> do
					Put.putWord16be (-1)
			) inventoryData
	
	putPacketContents (PlayerOnGround onGround) = do
		putBool onGround
	
	putPacketContents (PlayerSpawn x y z) = do
		Put.putWord32be (fromIntegral x)
		Put.putWord32be (fromIntegral y)
		Put.putWord32be (fromIntegral z)
	
	putPacketContents (PlayerPosition x y stance z onGround) = do
		BSF.putFloat64be x
		BSF.putFloat64be y
		BSF.putFloat64be stance
		BSF.putFloat64be z
		putBool onGround
		
	putPacketContents (PlayerLook rotation pitch onGround) = do
		BSF.putFloat32be rotation
		BSF.putFloat32be pitch
		putBool onGround
	
	putPacketContents (PlayerPositionLook x y stance z rotation pitch onGround) = do
		BSF.putFloat64be x
		BSF.putFloat64be y
		BSF.putFloat64be stance
		BSF.putFloat64be z
		BSF.putFloat32be rotation
		BSF.putFloat32be pitch
		putBool onGround
	
	putPacketContents (HoldItem entityId itemType) = do
		Put.putWord32be entityId
		Put.putWord16be itemType
	
	putPacketContents (AddInventoryItem itemType amount life) = do
		Put.putWord16be itemType
		Put.putWord8 amount
		Put.putWord16be life
	
	putPacketContents (ArmMovement entityId unk1) = do
		Put.putWord32be entityId
		Put.putWord8 (fromIntegral unk1)
	
	putPacketContents (NamedEntitySpawn entityId name x y z rotation pitch currentItem) = do
		Put.putWord32be entityId
		putPacketString name
		Put.putWord32be (fromIntegral x)
		Put.putWord32be (fromIntegral y)
		Put.putWord32be (fromIntegral z)
		Put.putWord8 (fromIntegral rotation)
		Put.putWord8 (fromIntegral pitch)
		Put.putWord16be (fromIntegral currentItem)
	
	putPacketContents (ItemSpawn entityId itemType unk1 x y z rotation pitch unk2) = do
		Put.putWord32be entityId
		Put.putWord16be itemType
		Put.putWord8 unk1
		Put.putWord32be (fromIntegral x)
		Put.putWord32be (fromIntegral y)
		Put.putWord32be (fromIntegral z)
		Put.putWord8 (fromIntegral rotation)
		Put.putWord8 (fromIntegral pitch)
		Put.putWord8 (fromIntegral unk2)
	
	putPacketContents (ObjectSpawn entityId objectType x y z) = do
		Put.putWord32be entityId
		Put.putWord8 objectType
		Put.putWord32be (fromIntegral x)
		Put.putWord32be (fromIntegral y)
		Put.putWord32be (fromIntegral z)
	
	putPacketContents (CollectItem collectedEntityId collectorEntityId) = do
		Put.putWord32be collectedEntityId
		Put.putWord32be collectorEntityId
	
	putPacketContents (MobSpawn entityId mobType x y z rotation pitch) = do
		Put.putWord32be entityId
		Put.putWord8 mobType
		Put.putWord32be (fromIntegral x)
		Put.putWord32be (fromIntegral y)
		Put.putWord32be (fromIntegral z)
		Put.putWord8 (fromIntegral rotation)
		Put.putWord8 (fromIntegral pitch)
		
	putPacketContents (DestroyEntity entityId) = do
		Put.putWord32be entityId
	
	putPacketContents (InitEnity entityId) = do
		Put.putWord32be entityId
		
	putPacketContents (EntityMove entityId x y z) = do
		Put.putWord32be entityId
		Put.putWord8 (fromIntegral x)
		Put.putWord8 (fromIntegral y)
		Put.putWord8 (fromIntegral z)
	
	putPacketContents (EntityLook entityId rotation pitch) = do
		Put.putWord32be entityId
		Put.putWord8 (fromIntegral rotation)
		Put.putWord8 (fromIntegral pitch)
	
	putPacketContents (EntityMoveLook entityId x y z rotation pitch) = do
		Put.putWord32be entityId
		Put.putWord8 (fromIntegral x)
		Put.putWord8 (fromIntegral y)
		Put.putWord8 (fromIntegral z)
		Put.putWord8 (fromIntegral rotation)
		Put.putWord8 (fromIntegral pitch)
	
	putPacketContents (EntityPositionLook entityId x y z rotation pitch) = do
		Put.putWord32be entityId
		Put.putWord32be (fromIntegral x)
		Put.putWord32be (fromIntegral y)
		Put.putWord32be (fromIntegral z)
		Put.putWord8 (fromIntegral rotation)
		Put.putWord8 (fromIntegral pitch)
	
	putPacketContents (PreChunk x z mode) = do
		Put.putWord32be (fromIntegral x)
		Put.putWord32be (fromIntegral z)
		putBool mode
	
	putPacketContents (MapChunk x y z sizeX sizeY sizeZ blockTypes blockMetadata blockLighting) = do
		Put.putWord32be (fromIntegral x)
		Put.putWord16be (fromIntegral y)
		Put.putWord32be (fromIntegral z)
		Put.putWord8 sizeX
		Put.putWord8 sizeY
		Put.putWord8 sizeZ
		let blockData = BSL.concat
			[ BSL.pack blockTypes
			, BSL.pack blockMetadata
			, doublePack blockLighting
			]
		let bs = Zlib.compress blockData
			
		Put.putWord32be (fromIntegral $ BSL.length bs)
		Put.putLazyByteString bs
		
	putPacketContents (MultiBlockChange x z size coordinates blockTypes blockMetadata) = do
		Put.putWord32be (fromIntegral x)
		Put.putWord32be (fromIntegral z)
		Put.putWord16be (fromIntegral size)
		mapM_ (\(coordX, coordY, coordZ) -> do
			let top = (shiftL coordX 4) + (coordY .&. 0)
			Put.putWord8 top
			Put.putWord8 coordZ) coordinates
		Put.putLazyByteString $ BSL.pack blockTypes
		Put.putLazyByteString $ BSL.pack blockMetadata
	
	putPacketContents (BlockChange x y z blockType blockMetadata) = do
		Put.putWord32be (fromIntegral x)
		Put.putWord8 (fromIntegral y)
		Put.putWord32be (fromIntegral z)
		Put.putWord8 blockType
		Put.putWord8 blockMetadata
	
	putPacketContents (Kick message) = do
		putPacketString message