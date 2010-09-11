module Minecraft.Map
	( MinecraftMap
	, MapBlockVector
	, MapChunkVector
	, MapBlock (..)
	, getMapChunk
	, getMapBlock
	, mapBlockVector
	, mapChunkVector
	, mapBlockVectorZ
	, mapBlockVectorX
	, mapBlockVectorY
	, mapChunkVectorZ
	, mapChunkVectorX
	, mapChunkVectorY
	, newMinecraftMap
	) where
import Minecraft.Map.Vector
import Minecraft.Block (Block)
import qualified Minecraft.Block as Block
import qualified Data.Judy as Judy
import Data.Word
import Data.Bits
import Debug.Trace

newtype MinecraftMap = MinecraftMap 
	{ minecraftMapBlocks :: Judy.JudyL MapBlock
	}
	deriving (Show)

newMinecraftMap :: IO MinecraftMap
newMinecraftMap = do
	mapBlocks <- Judy.new
	return MinecraftMap 
		{ minecraftMapBlocks = mapBlocks
		}

data MapBlock = MapBlock
	{ mapBlockBlock :: Block
	, mapBlockLighting :: Word8
	}
	deriving (Show, Eq)

instance Judy.JE MapBlock where
	toWord MapBlock
		{ mapBlockBlock = block
		, mapBlockLighting = lighting
		}
		= return $ (Block.blockId block)
		+ (256 * (Block.blockSecondaryData block .&. 0x0F))
		+ (256 * ((fromIntegral lighting) .&. 0xF0))
	fromWord i
		= return $ MapBlock
			{ mapBlockBlock = (Block.blockFromId (i .&. 0xFF) ((i `quot` 256) .&. 0x0F)) 
			, mapBlockLighting = fromIntegral ((i `quot` 256) .&. 0xF0)
			}

generateMapBlock :: MinecraftMap -> MapBlockVector -> IO MapBlock
generateMapBlock map@MinecraftMap {minecraftMapBlocks = judy} v
	= do
		let y = mapBlockVectorY v
		let mapBlock = case y `compare` 63 of
			LT -> MapBlock Block.Dirt 0x0F
			EQ -> MapBlock Block.Grass 0x0F
			GT -> MapBlock Block.Air 0x0F
		Judy.insert (case v of (MapBlockVector w) -> w) mapBlock judy
		return mapBlock

getMapBlock :: MinecraftMap -> MapBlockVector -> IO MapBlock
getMapBlock map@MinecraftMap {minecraftMapBlocks = judy} v = do
	mMapBlock <- (Judy.lookup (case v of (MapBlockVector w) -> w) judy)
	mapBlock <- maybe (generateMapBlock map v) return mMapBlock
	return mapBlock

getMapChunk :: MinecraftMap -> MapChunkVector -> IO [MapBlock]
getMapChunk map@MinecraftMap {minecraftMapBlocks = judy} v = do
	sequence $ case v of (MapChunkVector w) -> let v = (MapBlockVector (w * 16)) in getMapChunkY v (v + mapChunkYSizeVec)
	where
		getMapChunkY :: MapBlockVector -> MapBlockVector -> [IO MapBlock]
		getMapChunkY curr final =
			if curr /= final then
				getMapChunkZ curr (curr + mapChunkZSizeVec) (getMapChunkY (curr + oneBlockY) final)
			else
				[]
		
		getMapChunkZ :: MapBlockVector -> MapBlockVector -> [IO MapBlock] -> [IO MapBlock]
		getMapChunkZ curr final next =
			if curr /= final then
				getMapChunkX curr (curr + mapChunkXSizeVec) (getMapChunkZ (curr + oneBlockZ) final next)
			else
				next
		
		getMapChunkX :: MapBlockVector -> MapBlockVector -> [IO MapBlock] -> [IO MapBlock]
		getMapChunkX curr final next =
			if curr /= final then
				getMapBlock map curr : (getMapChunkX (curr + oneBlockX) final next)
			else
				next