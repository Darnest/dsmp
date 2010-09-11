module Minecraft.NBT.ByteString.Put
	( putNBT
	, putNBTWithGZipParams
	) where
import Minecraft.NBT.Structure
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Put as Put
import qualified Data.Binary.IEEE754 as BSF
import qualified Codec.Compression.GZip as GZip

putNBT :: NBT -> BSL.ByteString
putNBT nbt = GZip.compress $ Put.runPut $ putNamedTag nbt

putNBTWithGZipParams :: GZip.CompressParams -> NBT -> BSL.ByteString
putNBTWithGZipParams params nbt = GZip.compressWith params $ Put.runPut $ putNamedTag nbt

putTagTagType :: Tag -> Put.Put
putTagTagType = putTagType . tagType

putTagType :: Tag_Type -> Put.Put
putTagType = Put.putWord8 . fromIntegral . tagTypeToId

putTag :: Tag -> Put.Put
putTag tag = do
	putTagTagType tag
	putTagContents tag

putNamedTag :: NamedTag -> Put.Put
putNamedTag (NamedTagEnd tag) = do
	putTag tag
putNamedTag (NamedTag nameLength name tag) = do
	putTagTagType tag
	Put.putWord16be $ fromIntegral nameLength
	Put.putByteString name
	putTagContents tag

putTagContents :: Tag -> Put.Put
putTagContents Tag_End = return ()
putTagContents (Tag_Byte i) = Put.putWord8 $ fromIntegral i
putTagContents (Tag_Short i) = Put.putWord16be $ fromIntegral i
putTagContents (Tag_Int i) = Put.putWord32be $ fromIntegral i
putTagContents (Tag_Long i) = Put.putWord64be $ fromIntegral i
putTagContents (Tag_Float f) = BSF.putFloat32be f
putTagContents (Tag_Double f) = BSF.putFloat64be f
putTagContents (Tag_ByteArray length bs) = do
		Put.putWord32be $ fromIntegral length
		Put.putByteString bs
putTagContents (Tag_String length bs) = do
		Put.putWord16be $ fromIntegral length
		Put.putByteString bs
putTagContents (Tag_List tagType length tags) = do
		putTagType tagType
		Put.putWord32be $ fromIntegral length
		mapM_ putTagContents tags
putTagContents (Tag_Compound tags) = do
		mapM_ putNamedTag tags
		putTag Tag_End