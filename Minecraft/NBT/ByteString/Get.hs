module Minecraft.NBT.ByteString.Get 
	( getNBT
	, getNBTWithGZipParams
	, maybeGetNBT
	, maybeGetNBTWithGZipParams
	) where
import Minecraft.NBT.Structure
import Data.Int
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Trans
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Get as Get
import qualified Data.Binary.IEEE754 as BSF
import qualified Codec.Compression.GZip as GZip

parseError = (error "Minecraft.NBT.ByteString.Get: Could not parse NBT data")

getNBT :: BSL.ByteString -> NBT
getNBT bs = fromMaybe parseError (maybeGetNBT bs)

maybeGetNBT :: BSL.ByteString -> Maybe NBT
maybeGetNBT bs = Get.runGet (runMaybeT getNBTTag) (GZip.decompress bs)

getNBTWithGZipParams :: GZip.DecompressParams -> BSL.ByteString -> NBT
getNBTWithGZipParams params bs = fromMaybe parseError (maybeGetNBTWithGZipParams params bs)

maybeGetNBTWithGZipParams :: GZip.DecompressParams -> BSL.ByteString -> Maybe NBT
maybeGetNBTWithGZipParams params bs = Get.runGet (runMaybeT getNBTTag) (GZip.decompressWith params bs)

type MaybeGet a = MaybeT Get.Get a

getNBTTag :: MaybeGet NBT
getNBTTag = do
	namedTag <- getNamedTag :: MaybeGet NBT
	case namedTag of
		(NamedTag nameLength name (Tag_Compound namedTags)) -> return namedTag
		_                                                   -> fail ""

getTagContents :: Tag_Type -> MaybeGet Tag

getTagContents Tag_Type_End = return Tag_End

getTagContents Tag_Type_Byte = do
	i <- lift Get.getWord8
	return (Tag_Byte (fromIntegral i))

getTagContents Tag_Type_Short = do
	i <- lift Get.getWord16be
	return (Tag_Short (fromIntegral i))

getTagContents Tag_Type_Int = do
	i <- lift Get.getWord32be
	return (Tag_Int (fromIntegral i))

getTagContents Tag_Type_Long = do
	i <- lift Get.getWord64be
	return (Tag_Long (fromIntegral i))

getTagContents Tag_Type_Float = do
	f <- lift BSF.getFloat32be
	return (Tag_Float f)

getTagContents Tag_Type_Double = do
	f <- lift BSF.getFloat64be
	return (Tag_Double f)

getTagContents Tag_Type_ByteArray = do
	length <- lift Get.getWord32be
	bs <- lift $ Get.getByteString (fromIntegral length)
	return (Tag_ByteArray (fromIntegral length) bs)

getTagContents Tag_Type_String = do
	length <- lift Get.getWord16be
	bs <- lift $ Get.getByteString (fromIntegral length)
	return (Tag_String (fromIntegral length) bs)

getTagContents Tag_Type_List = do
	tagType <- getTagType :: MaybeGet Tag_Type
	length <- lift Get.getWord32be
	tags <- replicateM (fromIntegral length) (getTagContents tagType) :: MaybeGet [Tag]
	return (Tag_List tagType (fromIntegral length) tags)

getTagContents Tag_Type_Compound = do
	namedTags <- getTags :: MaybeGet [NamedTag]
	return (Tag_Compound namedTags)
	where
		getTags :: MaybeGet [NamedTag]
		getTags = do
		namedTag <- getNamedTag :: MaybeGet NamedTag
		case namedTag of
			(NamedTagEnd tag) -> return []
			_                 -> do
			namedTags <- getTags :: MaybeGet [NamedTag]
			return (namedTag:namedTags)

getTagType :: MaybeGet Tag_Type
getTagType = do
	tagTypeId <- lift Get.getWord8
	let tagType = tagTypeFromId (fromIntegral tagTypeId) :: Maybe Tag_Type
	maybe (fail "") return tagType

getTag :: MaybeGet Tag
getTag = do
	tagType <- getTagType :: MaybeGet Tag_Type
	getTagContents tagType

getNamedTagContents :: Tag_Type -> MaybeGet NamedTag
getNamedTagContents tagType@Tag_Type_End = do
	tag <- getTagContents tagType :: MaybeGet Tag
	return (NamedTagEnd tag)
getNamedTagContents tagType = do
	nameLength <- lift Get.getWord16be
	name <- lift $ Get.getByteString (fromIntegral nameLength) :: MaybeGet BS.ByteString
	tag <- getTagContents tagType :: MaybeGet Tag
	return (NamedTag (fromIntegral nameLength) name tag)

getNamedTag :: MaybeGet NamedTag
getNamedTag = do
	tagType <- getTagType :: MaybeGet Tag_Type
	getNamedTagContents tagType