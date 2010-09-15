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
	, mapBlockVectorToChunkVector
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
import Debug.Trace

data MapBlockVector = MapBlockVector Int32 Int32 Int32
	deriving (Eq, Show)

instance Num MapBlockVector where
	(MapBlockVector x1 y1 z1) + (MapBlockVector x2 y2 z2) = MapBlockVector (x1 + x2) (y1 + y2) (z1 + z2)
	(MapBlockVector x1 y1 z1) - (MapBlockVector x2 y2 z2) = MapBlockVector (x1 - x2) (y1 - y2) (z1 - z2)
	_ * _ = undefined
	negate = undefined
	abs = undefined
	signum = undefined
	fromInteger = undefined

newtype MapChunkVector = MapChunkVector MapBlockVector
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
bytesInWord = fromIntegral (log256 (fromIntegral (maxBound :: Word) + 1 :: Integer))
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
maxWordAbsHorizontal = fromIntegral $ floor $ sqrt $ fromIntegral $ ((maxBound :: Word) `quot` 8) `quot` mapHeight

maxChunkAbsHorizontal :: Int32
maxChunkAbsHorizontal = (`quot` chunkSizeHorizontal) $
	let n = fromIntegral maxWordAbsHorizontal :: Int32 in if fromIntegral n /= maxWordAbsHorizontal then
		(maxBound :: Int32)
		else
			n

maxAbsHorizontal :: Int32
--temporary until I get detection working
--maxAbsHorizontal = 3000
maxAbsHorizontal = maxChunkAbsHorizontal * chunkSizeHorizontal

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

mapBlockVector :: Int32 -> Int32 -> Int32 -> Maybe MapBlockVector
mapBlockVector x y z
	| x > mapMaxVectorX = Nothing
	| y > mapMaxVectorY = Nothing
	| z > mapMaxVectorZ = Nothing
	| x < mapMinVectorX = Nothing
	| y < mapMinVectorY = Nothing
	| z < mapMinVectorZ = Nothing
	| otherwise = Just $ MapBlockVector x y z

mapBlockVectorZ :: MapBlockVector -> Int32
mapBlockVectorZ (MapBlockVector _ _ z) = z

mapBlockVectorX :: MapBlockVector -> Int32
mapBlockVectorX (MapBlockVector x _ _) = x

mapBlockVectorY :: MapBlockVector -> Int32
mapBlockVectorY (MapBlockVector _ y _) = y

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
mapChunkVector x z = maybe Nothing (Just . MapChunkVector) $ mapBlockVector (x * 16) 0 (z * 16)

mapChunkVectorZ :: MapChunkVector -> Int32
mapChunkVectorZ (MapChunkVector v) = (mapBlockVectorZ v) `quot` 16

mapChunkVectorX :: MapChunkVector -> Int32
mapChunkVectorX (MapChunkVector v) = (mapBlockVectorX v) `quot` 16

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

xwidth = fromIntegral maxAbsHorizontal
ywidth = mapHeight
zwidth = xwidth
xywidth = xwidth * ywidth
xyzwidth = xywidth * zwidth

arrayIndex :: MapBlockVector -> Word
arrayIndex (MapBlockVector x y z) =
	let
		xlt0 = x < 0
		ylt0 = y < 0
		zlt0 = z < 0
		offset = if xlt0 then
				if ylt0 then
						if zlt0 then
								0
							else
								xyzwidth
					else
						if zlt0 then
								xyzwidth * 2
							else
								xyzwidth * 3
			else
				if ylt0 then
						if zlt0 then
								xywidth * 4
							else
								xyzwidth * 5
					else
						if zlt0 then
								xyzwidth * 6
							else
								xyzwidth * 7
		nx = fromIntegral $ abs x
		ny = fromIntegral $ abs y
		nz = fromIntegral $ abs z
		in
			offset + ny + nz * xywidth + nx * ywidth

mapBlockVectorToChunkVector :: MapBlockVector -> MapChunkVector
mapBlockVectorToChunkVector (MapBlockVector x y z)
	= MapChunkVector (MapBlockVector ((x `quot` 16) * 16) ((y `quot` 16) * 16) ((z `quot` 16) * 16))
			
setMapBlock :: MinecraftMap -> MapBlockVector -> MapBlock -> IO ()
setMapBlock (MinecraftMap mapBlocks) blockVector
	MapBlock
		{ mapBlockBlock = block
		, mapBlockLighting = lighting
		} = do
	let
		w = arrayIndex blockVector
		i = w `quot` blocksInWord
		d = w `rem` blocksInWord
		b = (Block.blockId block) * 256
		  + (fromIntegral lighting * 16)
		  + (Block.blockSecondaryData block)
	Judy.adjust (\w -> w .&. (complement $ d * 255) .|. (d * b)) i mapBlocks

getExistingMapBlock :: MinecraftMap -> MapBlockVector -> IO (Maybe MapBlock)
getExistingMapBlock (MinecraftMap mapBlocks) blockVector =
	let
		v = arrayIndex blockVector
		i = v `quot` blocksInWord
		d = v `rem` blocksInWord
		in do
			mw <- Judy.lookup i mapBlocks
			case mw of
				(Just w) -> do
					let
						w = arrayIndex blockVector
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
		let mapBlock = case y `compare` 5 of
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
getMapChunk map@MinecraftMap {minecraftMapBlocks = judy} (MapChunkVector v) = do
	sequence $ getMapChunkX xi yi zi
	where
		xi = mapBlockVectorX v
		yi = 0
		zi = mapBlockVectorZ v
		maxX = xi + 16
		maxY = 128
		maxZ = zi + 16
		getMapChunkX :: Int32 -> Int32 -> Int32 -> [IO MapBlock]
		getMapChunkX x y z =
			if x < maxX then
				getMapChunkZ x y z (getMapChunkX (x + 1) y z)
			else
				[]
		
		getMapChunkZ :: Int32 -> Int32 -> Int32 -> [IO MapBlock] -> [IO MapBlock]
		getMapChunkZ x y z next =
			if z < maxZ then
				getMapChunkY x y z (getMapChunkZ x y (z + 1) next)
			else
				next
		
		getMapChunkY :: Int32 -> Int32 -> Int32 -> [IO MapBlock] -> [IO MapBlock]
		getMapChunkY x y z next =
			if y < maxY then
				getMapBlock map (MapBlockVector x y z) : (getMapChunkY x (y + 1) z next)
			else
				next