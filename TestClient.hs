module Main
	( main
	) where
import qualified Minecraft.Network.Protocol.ClientPacket as ClientPacket
import qualified Minecraft.Network.Protocol.ServerPacket as ServerPacket
import Minecraft.Network.Protocol
import Control.Monad
import Network
import Data.IORef
import System.IO


hSendPacket h packet = do
	putStrLn $ "Sending packet: " ++ (show packet)
	hPutPacket h packet

main = do
	putStr "server: "
	hostname <- getLine
	putStr "port: "
	portStr <- getLine
	putStr "username"
	username <- getLine
	
	let port = read portStr :: Int
	
	putStrLn "Connecting to server..."
	
	handle <- connectTo hostname (PortNumber (fromIntegral port))
	hSetBuffering handle NoBuffering
	let sendPacket = hSendPacket handle
	
	packets <- (hGetPackets handle :: IO [ServerPacket.ServerPacket])
	packetIORef <- newIORef packets
	let getPacket = do
		ps <- readIORef packetIORef
		writeIORef packetIORef $ tail ps
		let packet = head ps
		putStrLn $ "Recieved packet: " ++ (show packet)
		return packet
	
	putStrLn "Connected to server."
	putStrLn "Sending handshake..."
	
	sendPacket ClientPacket.Handshake
		{ ClientPacket.handshakeUsername = username
		}
	putStrLn "Waiting for handshake..."
	
	handshakePacket <- getPacket
	
	forever getPacket