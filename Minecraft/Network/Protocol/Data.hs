module Minecraft.Network.Protocol.Data
	( BlockDirection (..)
	, putBlockDirection
	, getBlockDirection
	
	, DigStatus (..)
	, putDigStatus
	, getDigStatus
	
	, putPacketString
	, getPacketString
	
	, putBool
	, getBool
	
	, doublePack
	, doubleUnpack
	
	, Block
	) where

import Minecraft.Network.Protocol.Packet
import Minecraft.Block (Block, blockFromId, blockId)
import Minecraft.Item (Item, itemFromId, itemId)

import System.IO
import Control.Monad
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.UTF8 as BSUTF8
import qualified Data.ByteString.Lazy.UTF8 as BSLUTF8
import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable

putPacketString :: String -> Put.Put
putPacketString s = do
	let len = min (fromIntegral $ length s) (fromIntegral (maxBound :: Int16))
	Put.putWord16be $ fromIntegral len
	Put.putLazyByteString $ BSLUTF8.fromString $ take (fromIntegral len) s

getPacketString :: Get.Get String
getPacketString = do
	len <- Get.getWord16be
	bs <- Get.getByteString (fromIntegral len)
	return (BSUTF8.toString bs)

putBool :: Bool -> Put.Put
putBool bool = do
	Put.putWord8 (if(bool)
		then 1
		else 0)

getBool :: Get.Get Bool
getBool = do
	i <- Get.getWord8
	return (case i of
		0 -> False
		1 -> True
		_ -> Exception.throw ProtocolExceptionInvalidData)

data BlockDirection = XPos | XNeg | YPos | YNeg | ZPos | ZNeg
	deriving (Eq, Show)

putBlockDirection :: BlockDirection -> Put.Put
putBlockDirection dir
	= Put.putWord8 (case dir of
		XPos -> 5
		XNeg -> 4
		YPos -> 1
		YNeg -> 0
		ZPos -> 3
		ZNeg -> 2)

getBlockDirection :: Get.Get BlockDirection
getBlockDirection = do
	i <- Get.getWord8
	return (case i of
		5 -> XPos
		4 -> XNeg
		1 -> YPos
		0 -> YNeg
		3 -> ZPos
		2 -> ZNeg
		_ -> Exception.throw ProtocolExceptionInvalidData)

data DigStatus = DigStart | DigStop | DigFinish
	deriving (Eq, Show)

putDigStatus :: DigStatus -> Put.Put
putDigStatus dir
	= Put.putWord8 (case dir of
		DigStart  -> 1
		DigStop   -> 2
		DigFinish -> 3)

getDigStatus :: Get.Get DigStatus
getDigStatus = do
	i <- Get.getWord8
	return (case i of
		1 -> DigStart
		2 -> DigStop
		3 -> DigFinish
		_ -> Exception.throw ProtocolExceptionInvalidData)

doublePack :: [Word8] -> BSL.ByteString
doublePack = 
	BSL.pack . packList
	where
		packList [] = []
		packList (a:b:xs) = (shiftL a 4) + (b .&. 0x0F) : (packList xs)

doubleUnpack :: BSL.ByteString -> [Word8]
doubleUnpack bs
	| BSL.null bs = []
	| otherwise = let b = BSL.head bs in
		(shiftR b 4) : (b .&. 0x0F) : doubleUnpack (BSL.tail bs)