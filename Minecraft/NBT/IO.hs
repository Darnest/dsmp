module Minecraft.NBT.IO 
	( hPutNBT
	, hGetNBT
	, hGetContentsNBT
	, readNBTFile
	, writeNBTFile
	) where
import Minecraft.NBT.ByteString.Get
import Minecraft.NBT.ByteString.Put
import Minecraft.NBT
import System.IO
import Data.ByteString.Lazy as BSL

hPutNBT :: Handle -> NBT -> IO ()
hPutNBT h = (BSL.hPut h) . putNBT

hGetNBT :: Handle -> Int -> IO NBT
hGetNBT h i = do
	bs <- (BSL.hGet h i)
	return (getNBT bs)

hGetContentsNBT :: Handle -> IO NBT
hGetContentsNBT h = do
	bs <- (BSL.hGetContents h)
	return (getNBT bs)

readNBTFile :: FilePath -> IO NBT
readNBTFile f = do
	bs <- BSL.readFile f
	return (getNBT bs)

writeNBTFile :: FilePath -> NBT -> IO ()
writeNBTFile f nbt = do
	BSL.writeFile f (putNBT nbt)