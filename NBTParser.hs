import Minecraft.NBT.IO
import System.IO
import System.Environment

main = do
	args <- getArgs
	nbt <- case args of
		[] -> do
			hGetContentsNBT stdin
		(inputFile:_) -> do
			readNBTFile inputFile
	print nbt