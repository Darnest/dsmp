module Minecraft.Network.Protocol.Packet
	( Packet (..)
	, ProtocolException (..)
	, bsPutPacket
	, bsPutPackets
	, bsGetPacket
	, bsGetPackets
	, hPutPacket
	, hPutPackets
	, hGetPackets
	, protocolVersion
	) where
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import Control.Monad
import Control.Concurrent
import Data.Int
import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable

protocolVersion :: Integral a => a
protocolVersion = 2

class Show packet => Packet packet where
	packetId :: packet -> Int8
		
	putPacketContents :: packet -> Put.Put
	
	getPacketContents :: Int8 -> Get.Get packet

data ProtocolException
	= ProtocolExceptionNotSupported
	| ProtocolExceptionInvalidData
	| ProtocolExceptionEOF
	| ProtocolExceptionIO Exception.IOException
	deriving (Show, Eq)

instance Typeable.Typeable ProtocolException where
	typeOf e = Typeable.mkTyConApp tyCon []
		where
			tyCon = Typeable.mkTyCon "Minecraft.Protocol.Packet.ProtocolException"

instance Exception.Exception ProtocolException

withProtocolExceptions :: a -> a
withProtocolExceptions
	= Exception.mapException (\e -> ProtocolExceptionIO e)
	--The only errors should be from Binary.Get and indicate EOF
	. (Exception.mapException ((\e -> ProtocolExceptionEOF) :: Exception.ErrorCall -> ProtocolException))
	
bsPutPacket :: Packet packet => packet -> Put.Put
bsPutPacket packet = do
	Put.putWord8 (fromIntegral (packetId packet))
	putPacketContents packet

bsPutPackets :: Packet packet => [packet] -> Put.Put
bsPutPackets packets
	= mapM_ bsPutPacket packets

bsGetPacket :: Packet packet => Get.Get packet
bsGetPacket = withProtocolExceptions $ do
	packetId <- Get.getWord8
	getPacketContents (fromIntegral packetId)

bsGetPackets :: Packet packet => Get.Get [packet]
bsGetPackets = withProtocolExceptions $ sequence $ repeat bsGetPacket

hPutPacket :: Packet packet => Handle -> packet -> IO ()
hPutPacket h packet
	= do
		BSL.hPut h $ Put.runPut $ bsPutPacket packet

hPutPackets :: Packet packet => Handle -> [packet] -> IO ()
hPutPackets _ [] = return ()
hPutPackets h (packet:packets) = do
	hPutPacket h packet
	hPutPackets h packets

hGetPackets :: Packet packet => Handle -> IO [packet]
hGetPackets h = withProtocolExceptions $ do
	bs <- BSL.hGetContents h
	return $ getPackets bs
	where
		getPackets :: Packet packet => BSL.ByteString -> [packet]
		getPackets bs =
			case Get.runGetState bsGetPacket bs maxBound of
				(a, rest, _) -> a:(getPackets rest)