module Minecraft.Map
	( MinecraftMap
	, MapBlock (..)
	, MapBlockVector
	, MapChunkVector
	, mapBlockVector
	, mapChunkVector
	, mapChunkVectorX
	, mapChunkVectorZ
	, mapBlockVectorX
	, mapBlockVectorY
	, mapBlockVectorZ
	, mapMaxVectorX
	, mapMaxVectorY
	, mapMaxVectorZ
	, mapMaxChunkVectorX
	, mapMaxChunkVectorZ
	, mapMinVectorX
	, mapMinVectorY
	, mapMinVectorZ
	, mapMinChunkVectorX
	, mapMinChunkVectorZ
	, getMapBlock
	, setMapBlock
	, newMinecraftMap
	, getMapChunk
	) where
import Data.Judy (JudyL, JE)
import Minecraft.Block
import Minecraft.Block (Block)
import qualified Minecraft.Block as Block
import qualified Data.Judy as Judy
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe

newtype MapBlockVector = MapBlockVector Word
	deriving (Eq, Show)

instance Num MapBlockVector where
	(MapBlockVector w1) + (MapBlockVector w2) = MapBlockVector $ w1 + w2
						
	(MapBlockVector w1) - (MapBlockVector w2) = MapBlockVector $ w1 - w2
	_ * _ = undefined
	negate = undefined
	abs = undefined
	signum = undefined
	fromInteger = undefined

newtype MapChunkVector = MapChunkVector Word
	deriving (Eq, Show)

instance Num MapChunkVector where
	(MapChunkVector w1) + (MapChunkVector w2) = MapChunkVector (w1 + w2)
	(MapChunkVector w1) - (MapChunkVector w2) = MapChunkVector (w1 - w2)
	_ * _ = undefined
	negate = undefined
	abs = undefined
	signum = undefined
	fromInteger = undefined

chunkSizeHorizontal :: Integral a => a
chunkSizeHorizontal = 16

chunkBlocks :: Integral a => a
chunkBlocks = (chunkSizeHorizontal ^ 2) * mapHeight

mapBlockSize :: Integral a => a
mapBlockSize = 2

blocksInWord :: Integral a => a
blocksInWord = bytesInWord `quot` mapBlockSize

bytesInWord :: Integral a => a
bytesInWord = fromIntegral (log256 maxBound :: Word)
	where
		log256h 0 i = i - 1
		log256h 1 i = i
		log256h n i = log256h (n `quot` 256) (i + 1)
		log256 :: Integral a => a -> a
		log256 1 = 0
		log256 n = log256h n 0

mapHeight :: Integral a => a
mapHeight = 128

mapMaxVectorY :: Int32
mapMaxVectorY = mapHeight - 1

mapMinVectorY :: Int32
mapMinVectorY = 0

maxWordAbsHorizontal :: Word
maxWordAbsHorizontal = floor $ sqrt $ fromIntegral $ (`quot` mapHeight) $ (maxBound :: Word)

maxChunkAbsHorizontal :: Int32
maxChunkAbsHorizontal = (`quot` chunkBlocks) $
	min (fromIntegral maxWordAbsHorizontal) (maxBound :: Int32)

maxAbsHorizontal :: Int32
maxAbsHorizontal = maxChunkAbsHorizontal * chunkBlocks

maxHorizontal :: Int32
maxHorizontal = maxAbsHorizontal - 1

minHorizontal :: Int32
minHorizontal = negate maxAbsHorizontal

mapMaxVectorX :: Int32
mapMaxVectorX = maxHorizontal

mapMaxVectorZ :: Int32
mapMaxVectorZ = maxHorizontal

mapMinVectorX :: Int32
mapMinVectorX = minHorizontal

mapMinVectorZ :: Int32
mapMinVectorZ = minHorizontal

mapMaxVectorXY = (mapMaxVectorY * mapMaxVectorX)

mapBlockVector :: Int32 -> Int32 -> Int32 -> Maybe MapBlockVector
mapBlockVector x y z
	| x > mapMaxVectorX = Nothing
	| y > mapMaxVectorY = Nothing
	| z > mapMaxVectorZ = Nothing
	| x < mapMinVectorX = Nothing
	| y < mapMinVectorY = Nothing
	| z < mapMinVectorZ = Nothing
	| otherwise = Just $ MapBlockVector (fromIntegral (y + (x * mapMaxVectorY) + (z * mapMaxVectorXY)))

mapBlockVectorZ :: MapBlockVector -> Int32
mapBlockVectorZ (MapBlockVector v) = (fromIntegral v) `quot` (mapMaxVectorX * mapMaxVectorY)

mapBlockVectorX :: MapBlockVector -> Int32
mapBlockVectorX (MapBlockVector v) = ((fromIntegral v) `quot` mapMaxVectorY) `div` mapMaxVectorX

mapBlockVectorY :: MapBlockVector -> Int32
mapBlockVectorY (MapBlockVector v) = (fromIntegral v) `div` mapMaxVectorY

maxChunkHorizontal :: Int32
maxChunkHorizontal = maxChunkAbsHorizontal

minChunkHorizontal :: Int32
minChunkHorizontal = negate maxAbsHorizontal

mapMaxChunkVectorX :: Int32
mapMaxChunkVectorX = maxChunkHorizontal

mapMaxChunkVectorZ :: Int32
mapMaxChunkVectorZ = maxChunkHorizontal

mapMinChunkVectorX :: Int32
mapMinChunkVectorX = minChunkHorizontal

mapMinChunkVectorZ :: Int32
mapMinChunkVectorZ = minChunkHorizontal

mapChunkVector :: Int32 -> Int32 -> Maybe MapChunkVector
mapChunkVector x z
	| x > mapMaxChunkVectorX = Nothing
	| z > mapMaxChunkVectorZ = Nothing
	| x < mapMinChunkVectorX = Nothing
	| z < mapMinChunkVectorZ = Nothing
	| otherwise = Just $ MapChunkVector (fromIntegral $ (x * mapMaxVectorY) + (z * mapMaxVectorXY) - 1)

mapChunkVectorZ :: MapChunkVector -> Int32
mapChunkVectorZ (MapChunkVector v) = (fromIntegral v) `quot` (mapMaxVectorX * mapMaxVectorY)

mapChunkVectorX :: MapChunkVector -> Int32
mapChunkVectorX (MapChunkVector v) = ((fromIntegral v) `quot` mapMaxVectorY) `div` mapMaxVectorX

mapChunkVectorY :: MapChunkVector -> Int32
mapChunkVectorY (MapChunkVector v) = (fromIntegral v) `div` mapMaxVectorY

newtype MinecraftMap = MinecraftMap 
	{ minecraftMapBlocks :: Judy.JudyL Word
	}

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

setMapBlock :: MinecraftMap -> MapBlockVector -> MapBlock -> IO ()
setMapBlock (MinecraftMap mapBlocks) (MapBlockVector w)
	MapBlock
		{ mapBlockBlock = block
		, mapBlockLighting = lighting
		} = do
	let
		i = w `quot` blocksInWord
		d = w `div` blocksInWord
		b = (Block.blockId block) * 256
		  + (fromIntegral lighting * 16)
		  + (Block.blockSecondaryData block)
	Judy.adjust (\w -> w .&. (complement $ d * 255) .|. (d * b)) i mapBlocks

getExistingMapBlock :: MinecraftMap -> MapBlockVector -> IO (Maybe MapBlock)
getExistingMapBlock (MinecraftMap mapBlocks) (MapBlockVector v) =
	let
		i = v `quot` blocksInWord
		d = v `div` blocksInWord
		in do
			mw <- Judy.lookup i mapBlocks
			case mw of
				(Just w) -> do
					let
						b = (w `quot` d)
						blockId = (b `quot` 256) .&. 0xFF
						metadata = b .&. 0x0F
						lighting = b .&. 0xF0
					return $ Just MapBlock
						{ mapBlockBlock = Block.blockFromId blockId metadata
						, mapBlockLighting = fromIntegral lighting
						}
				Nothing -> return Nothing

generateMapBlock :: MinecraftMap -> MapBlockVector -> IO MapBlock
generateMapBlock map@MinecraftMap {minecraftMapBlocks = mMapBlocks} v
	= do
		let y = mapBlockVectorY v
		let mapBlock = case y `compare` 63 of
			LT -> MapBlock Block.Dirt 0x0F
			EQ -> MapBlock Block.Grass 0x0F
			GT -> MapBlock Block.Air 0x0F
		setMapBlock map v mapBlock
		return mapBlock

getMapBlock :: MinecraftMap -> MapBlockVector -> IO MapBlock
getMapBlock map@MinecraftMap {minecraftMapBlocks = mMapBlocks} v = do
	mMapBlock <- getExistingMapBlock map v
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
		mapChunkXSize = (oneBlockX * 16)

		multv w n = case w of (MapBlockVector w) -> (MapBlockVector (w * n))
		mapChunkXSizeVec = multv oneBlockX 16
		mapChunkYSizeVec = multv oneBlockY 128
		mapChunkZSizeVec = multv oneBlockZ 16
		mapChunkSizeVec = (oneBlockX `multv` (16 * 16 * 128)) + (oneBlockZ `multv` (16 * 128)) + (oneBlockY `multv` 128)
		
		oneBlockX = fromJust $ mapBlockVector 1 0 0
		oneBlockY = fromJust $ mapBlockVector 0 1 0
		oneBlockZ = fromJust $ mapBlockVector 0 0 1