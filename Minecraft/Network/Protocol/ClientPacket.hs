module Minecraft.Network.Protocol.ClientPacket
	( ClientPacket (..)
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

data ClientPacket
	= KeepAlive
	| Login
		{ loginProtocolVersion :: Word32
		, loginUserName :: String
		, loginPassword :: String
		}
	| Handshake
		{ handshakeUsername :: String
		}
	| Chat
		{ chatMessage :: String
		}
	| PlayerOnGround
		{ playerOnGround :: Bool
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
	| Dig 
		{ digStatus :: DigStatus
		, digX :: Int32
		, digY :: Int8
		, digZ :: Int32
		, unknown1 :: Int8
		}
	| Place
		{ placeBlockType :: Word16
		, placeX :: Int32
		, placeY :: Int8
		, placeZ :: Int32
		, direction :: BlockDirection
		}
	| HoldItem
		{ entityId :: Word32
		, itemType :: Word16
		}
	| ArmMovement
		{ armMovementEntityId :: Word32
		, armMovementUnknown1 :: Int8
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
	| RequestEnity
		{ requestEntityId :: Word32
		}
	| Disconnect
		{ disconnectMessage :: String
		}
	deriving (Eq, Show)

instance Packet ClientPacket where
	packetId KeepAlive {}          = 0x00
	packetId Login {}              = 0x01
	packetId Handshake {}          = 0x02
	packetId Chat {}               = 0x03
	packetId PlayerOnGround {}     = 0x0A
	packetId PlayerPosition {}     = 0x0B
	packetId PlayerLook {}         = 0x0C
	packetId PlayerPositionLook {} = 0x0D
	packetId Dig {}                = 0x0E
	packetId Place {}              = 0x0F
	packetId HoldItem {}           = 0x10
	packetId ArmMovement {}        = 0x12
	packetId ItemSpawn {}          = 0x15
	packetId RequestEnity {}       = 0x1E
	packetId Disconnect {}         = 0xFF
	
	getPacketContents 0x00 = return KeepAlive
	
	getPacketContents 0x01 = do
		version <- Get.getWord32be
		username <- getPacketString
		password <- getPacketString
		return (Login version username password)
	
	getPacketContents 0x02 = do
		username <- getPacketString
		return (Handshake username)
	
	getPacketContents 0x03 = do
		message <- getPacketString
		return (Chat message)
	
	getPacketContents 0x0A = do
		playerOnGround <- getBool
		return (PlayerOnGround playerOnGround)
	
	getPacketContents 0x0B = do
		x <- BSF.getFloat64be
		y <- BSF.getFloat64be
		stance <- BSF.getFloat64be
		z <- BSF.getFloat64be
		playerOnGround <- getBool
		return (PlayerPosition x y stance z playerOnGround)
	
	getPacketContents 0x0C = do
		rotation <- BSF.getFloat32be
		pitch <- BSF.getFloat32be
		playerOnGround <- getBool
		return (PlayerLook rotation pitch playerOnGround)
	
	getPacketContents 0x0D = do
		x <- BSF.getFloat64be
		y <- BSF.getFloat64be
		stance <- BSF.getFloat64be
		z <- BSF.getFloat64be
		rotation <- BSF.getFloat32be
		pitch <- BSF.getFloat32be
		playerOnGround <- getBool
		return (PlayerPositionLook x y stance z rotation pitch playerOnGround)
	
	getPacketContents 0x0E = do
		status <- getDigStatus
		x <- Get.getWord32be
		y <- Get.getWord8
		z <- Get.getWord32be
		unk1 <- Get.getWord8
		return (Dig status (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral unk1))
	
	getPacketContents 0x0F = do
		itemType <- Get.getWord16be
		x <- Get.getWord32be
		y <- Get.getWord8
		z <- Get.getWord32be
		direction <- getBlockDirection
		return (Place itemType (fromIntegral x) (fromIntegral y) (fromIntegral z) direction)
	
	getPacketContents 0x10 = do
		entityId <- Get.getWord32be
		itemType <- Get.getWord16be
		return (HoldItem entityId itemType)
	
	getPacketContents 0x12 = do
		entityId <- Get.getWord32be
		unk1 <- Get.getWord8
		return (ArmMovement entityId (fromIntegral unk1))
	
	getPacketContents 0x15 = do
		entityId <- Get.getWord32be
		itemType <- Get.getWord16be
		unk1 <- Get.getWord8
		x <- Get.getWord32be
		y <- Get.getWord32be
		z <- Get.getWord32be
		rotation <- Get.getWord8
		pitch <- Get.getWord8
		unk2 <- Get.getWord8
		return (ItemSpawn entityId itemType unk1 (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral rotation) (fromIntegral pitch) (fromIntegral unk2))
	
	getPacketContents 0x1E = do
		entityId <- Get.getWord32be
		return (RequestEnity entityId)
	
	getPacketContents 0xFF = do
		message <- getPacketString
		return (Disconnect message)
	
	putPacketContents (Handshake username) = do
		putPacketString username
	putPacketContents (Login version username password) = do
		Put.putWord32be version
		putPacketString username
		putPacketString password
	putPacketContents _ = undefined