module Minecraft.Map.Vector where
import Data.Word
import Data.Int

newtype MapBlockVector = MapBlockVector Word
	deriving (Eq, Show)

instance Num MapBlockVector where
	(MapBlockVector w1) + (MapBlockVector w2) = MapBlockVector (w1 + w2)
	(MapBlockVector w1) - (MapBlockVector w2) = MapBlockVector (w1 + w2)
	_ * _ = undefined
	negate (MapBlockVector w) = MapBlockVector (negate w)
	abs = undefined
	signum = undefined
	fromInteger = undefined

newtype MapChunkVector = MapChunkVector Word
	deriving (Eq, Show)

instance Num MapChunkVector where
	(MapChunkVector w1) + (MapChunkVector w2) = MapChunkVector (w1 + w2)
	(MapChunkVector w1) - (MapChunkVector w2) = MapChunkVector (w1 + w2)
	_ * _ = undefined
	negate (MapChunkVector w) = MapChunkVector (negate w)
	abs = undefined
	signum = undefined
	fromInteger = undefined


maxHorizontal = floor $ sqrt $ fromIntegral (maxBound :: Int32)
minHorizontal = negate $ floor $ sqrt $ fromIntegral $ negate (minBound :: Int32)
maxX = maxHorizontal
maxY = (127 :: Int32)
maxZ = maxHorizontal

minX = minHorizontal
minY = (0 :: Int32)
minZ = minHorizontal

mapBlockVector :: Int32 -> Int32 -> Int32 -> MapBlockVector
mapBlockVector x y z = MapBlockVector (fromIntegral (y + (x * maxY) + (z * (maxY * maxX))))

mapBlockVectorZ (MapBlockVector v) = (fromIntegral v) `quot` (maxX * maxY)
mapBlockVectorX (MapBlockVector v) = ((fromIntegral v) `quot` maxY) `div` maxX
mapBlockVectorY (MapBlockVector v) = (fromIntegral v) `div` maxY

mapChunkVectorZ (MapChunkVector v) = (fromIntegral v) `quot` (maxX * maxY)
mapChunkVectorX (MapChunkVector v) = ((fromIntegral v) `quot` maxY) `div` maxX
mapChunkVectorY (MapChunkVector v) = (fromIntegral v) `div` maxY

mapChunkXSize = (oneBlockX * 16)

multv w n = case w of (MapBlockVector w) -> (MapBlockVector (w * n))
mapChunkXSizeVec = multv oneBlockX 16
mapChunkYSizeVec = multv oneBlockY 128
mapChunkZSizeVec = multv oneBlockZ 16
mapChunkSizeVec = (oneBlockX `multv` (16 * 16 * 128)) + (oneBlockZ `multv` (16 * 128)) + (oneBlockY `multv` 128)

oneBlockX = mapBlockVector 1 0 0
oneBlockY = mapBlockVector 0 1 0
oneBlockZ = mapBlockVector 0 0 1

mapChunkVector :: Int32 -> Int32 -> Int32 -> MapChunkVector
mapChunkVector x y z = MapChunkVector (fromIntegral (y + (x * maxY) + (z * (maxY * maxX))))