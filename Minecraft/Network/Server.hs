module Minecraft.Network.Server
	( Server
	, ServerConfig (..)
	, startServer
	, waitForServer
	, serverRunning
	, stopServer
	) where

import Minecraft.Network.Protocol
import qualified Minecraft.Network.Protocol.ServerPacket as ServerPacket
import qualified Minecraft.Network.Protocol.ClientPacket as ClientPacket
import Minecraft.Network.Client.ClientActionResult
import Minecraft.Network.Minecraft_Net
import Minecraft.Map
import Minecraft.World
import Minecraft.Player
import Minecraft.World
import Minecraft.Entity
import Minecraft.Block (blockId, blockSecondaryData)
import Minecraft.Server.ServerId

import qualified Network
import qualified Network.Socket as Socket
import Control.Concurrent
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSUTF8
import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.StoppableChan
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.Trans
import Data.Maybe

type ClientAction a = ClientActionT IO a

data Client
	= Client
		{ clientHandle :: Handle
		, clientHostName :: Network.HostName
		, clientPort :: Network.PortNumber
		, clientPacketWriteChan :: SChan ServerPacket
		, clientPacketReadChan :: SChan ClientPacket
		, clientDisconnectAction :: MVar (ClientActionResult ())
		}

data PlayerClient
	= PlayerClient
		{ playerClientPlayer :: Player
		, playerClientClient :: Client
		}

data PlayerWorldClient
	= PlayerWorldClient
		{ playerWorldClientPlayer :: Player
		, playerWorldClientClient :: Client
		, playerWorldClientPlayerEntity :: PlayerEntity
		, playerWorldClientWorld :: World
		}

class ClientBehavior client where
	sendPacket :: client -> ServerPacket -> ClientAction ()
	getPacket :: client -> ClientAction ClientPacket
	getNonPingPacket :: client -> ClientAction ClientPacket
	getNonPingPacket client = do
		packet <- getPacket client
		case packet of
			(ClientPacket.KeepAlive) -> getNonPingPacket client
			_ -> return packet
	
	stopClient :: client -> ClientActionResult () -> ClientAction a
	kickClient :: client -> String -> ClientAction a

instance ClientBehavior Client where
	sendPacket client@Client
		{ clientPacketWriteChan = wChan
		, clientDisconnectAction = dAction
		} packet = do
		lift $ putStrLn ("sending packet to wchan " ++ (show packet))
		succ <- lift $ writeSChan wChan packet
		lift $ putStrLn ("sent packet to wchan " ++ (show packet))
		if succ then
				return ()
			else do
				disconnect <- lift $ readMVar dAction
				failedClientActionResultT disconnect
	getPacket client@Client
		{ clientPacketReadChan = rChan
		, clientDisconnectAction = dAction
		} = do
		lift $ putStrLn "waiting for packet from rchan"
		mPacket <- lift $ readSChan rChan
		return ()
		case mPacket of
			(Just packet) -> do
				lift $ putStrLn $ "got packet from rchan: " ++ (show packet)
				case packet of
					(ClientPacket.Disconnect message) -> clientActionResultT (ClientDisconnected message)
					_ -> return packet
			Nothing -> do
				disconnect <- lift $ readMVar dAction
				failedClientActionResultT disconnect
	
	stopClient client@Client
		{ clientPacketWriteChan = wChan
		, clientPacketReadChan = rChan
		, clientDisconnectAction = mAction
		} action = do
			succ <- lift $ tryPutMVar mAction action
			if succ then do
					lift $ do
						forkIO $ do
							closeSChan wChan
							return ()
						forkIO $ do
							closeSChan rChan
							return ()
					failedClientActionResultT $ action
				else do
					disconnect <- lift $ readMVar mAction
					failedClientActionResultT disconnect

	kickClient client s = stopClient client $ ClientKicked s
	
unexpectedDisconnectClient :: ClientBehavior client => client -> String -> ClientAction a
unexpectedDisconnectClient client s = stopClient client $ UnexpectedClientDisconnect s
	

instance ClientBehavior PlayerClient where
	sendPacket = sendPacket . playerClientClient
	getPacket = getPacket . playerClientClient
	stopClient = stopClient . playerClientClient
	kickClient = kickClient . playerClientClient

instance ClientBehavior PlayerWorldClient where
	sendPacket = sendPacket . playerWorldClientClient
	getPacket = getPacket . playerWorldClientClient
	stopClient = stopClient . playerWorldClientClient
	kickClient = kickClient . playerWorldClientClient

startClient :: Handle -> Network.HostName -> Network.PortNumber -> IO Client
startClient handle hostName port = do
	putStrLn "starting client..."
	rChan <- newSChan
	wChan <- newSChan
	putStrLn "starting chans"
	wChanContents <- getSChanContents wChan
	putStrLn "starting chans 1"
	forkIO $ hPutPackets handle wChanContents
	putStrLn "starting chans 2"
	packets <- hGetPackets handle
	putStrLn "finished chans"
	forkIO $ do
		putStrLn "writing packets"
		writeList2SChan rChan packets
		return ()
	
	disconnectAction <- newEmptyMVar
	
	return Client
		{ clientHandle = handle
		, clientHostName = hostName
		, clientPort = port
		, clientPacketReadChan = rChan
		, clientPacketWriteChan = wChan
		, clientDisconnectAction = disconnectAction
		}

authenticateClient :: String -> Client -> ClientAction PlayerClient
authenticateClient serverId client = do
	handshakePacket <- getNonPingPacket client
	lift $ putStrLn "got handshake packet"
	case handshakePacket of
		(ClientPacket.Handshake hUsername) -> return ()
		_ -> kickClient client "Unexpected data"
	lift $ putStrLn ("sending handshake...")
	sendPacket client ServerPacket.Handshake
		{ ServerPacket.handshakeServerId = serverId
		}
	lift $ putStrLn "waiting for login"
	loginPacket <- getNonPingPacket client
	lift $ putStrLn "got login packet in authenticateClient"
	(version, username, password) <-
		case loginPacket of
			(ClientPacket.Login version username password) -> return (version, username, password)
			_ -> do
				kickClient client "Unexpected data"
	
	if version /= protocolVersion then
			kickClient client "Usupported protocol version"
		else
			return ()
	lift $ putStrLn "verifying username"
	mVerify <- lift $ verifyUsername serverId username
	lift $ putStrLn "verifying username web query complete"
	maybe
		(kickClient client "Username does not verify")
		(\b -> if b then
				return ()
			else
				kickClient client "Attempt to verify username failed"
		)
		mVerify
	lift $ putStrLn "preparing to send login packet"
	sendPacket client (ServerPacket.Login 0 "" "")
	return PlayerClient
		{ playerClientPlayer = Player
			{ playerUsername = username
			, playerHeldItem = Nothing
			}
		, playerClientClient = client
		}

sendMapChunk :: PlayerWorldClient -> MapChunkVector -> ClientAction ()
sendMapChunk PlayerWorldClient
	{ playerWorldClientWorld = world@World
		{ worldMap = minecraftMap
		}
	, playerWorldClientClient = client
	} v = do
		let
			x = mapChunkVectorX v
			z = mapChunkVectorZ v
		
		sendPacket client ServerPacket.PreChunk
			{ ServerPacket.preChunkX = x
			, ServerPacket.preChunkZ = z
			, ServerPacket.preChunkMode = True
			}
		chunk <- lift $ unsafeInterleaveIO $ getMapChunk minecraftMap v
		let blocks = map mapBlockBlock chunk
		sendPacket client ServerPacket.MapChunk
			{ ServerPacket.mapChunkX = x
			, ServerPacket.mapChunkY = 0
			, ServerPacket.mapChunkZ = z
			, ServerPacket.mapChunkXSize = 15
			, ServerPacket.mapChunkYSize = 127
			, ServerPacket.mapChunkZSize = 15
			, ServerPacket.mapChunkBlockTypes = map blockId blocks
			, ServerPacket.mapChunkBlockMetadata = map blockSecondaryData blocks
			, ServerPacket.mapChunkBlockLighting = map mapBlockLighting chunk
			}

		
sendClientWorld :: World -> PlayerClient -> ClientAction PlayerWorldClient
sendClientWorld world@World
	{ worldMap = map
	} PlayerClient
	{ playerClientPlayer = player
	, playerClientClient = client
	} = do
		playerEntity <- lift $ addWorldPlayer world player
		let playerWorldClient = PlayerWorldClient
			{ playerWorldClientPlayer = player
			, playerWorldClientPlayerEntity = playerEntity
			, playerWorldClientClient = client
			, playerWorldClientWorld = world
			}
		let blockVector = entityMapBlockVector playerEntity
		sendPacket client ServerPacket.PlayerSpawn
			{ ServerPacket.playerSpawnX = mapBlockVectorX blockVector
			, ServerPacket.playerSpawnY = mapBlockVectorY blockVector
			, ServerPacket.playerSpawnZ = mapBlockVectorZ blockVector
			}
		sendMapChunk playerWorldClient (fromJust $ mapChunkVector 0 0)
		sendPacket client ServerPacket.PlayerPositionLook
			{ ServerPacket.playerPositionLookX = entityX playerEntity
			, ServerPacket.playerPositionLookY = entityY playerEntity
			, ServerPacket.playerPositionLookStance = 65.620000004768372 
			, ServerPacket.playerPositionLookZ = entityZ playerEntity
			, ServerPacket.playerPositionLookRotation = realToFrac $ entityRotation playerEntity
			, ServerPacket.playerPositionLookPitch = realToFrac $ entityPitch playerEntity
			, ServerPacket.playerPositionLookOnGround = True
			}
		return playerWorldClient

actPlayerWorldClient :: PlayerWorldClient -> ClientAction ()
actPlayerWorldClient client = forever $ do
	lift $ putStrLn "waiting for a packet"
	packet <- getNonPingPacket client
	lift $ putStrLn "got packet"
	case packet of
		_ -> return ()

data ServerConfig
	= ServerConfig
		{ serverConfigPort :: Network.PortID
		}
	

data Server
	= Server
		{ serverConfig :: ServerConfig
		, serverSocket :: Network.Socket
		, serverWait :: MVar ()
		, serverId :: String
		}

startServer :: ServerConfig -> IO Server
startServer config@ServerConfig {serverConfigPort = port}
	= Network.withSocketsDo $ do
		socket <- Network.listenOn port
		wait <- newEmptyMVar
		
		serverId <- generateServerId
		let server = (Server config socket wait serverId)
		world <- newWorld
		forkIO $ forever $ do
			putStrLn "waiting for client"
			(handle, hostName, port) <- Network.accept socket
			hSetBuffering handle NoBuffering
			putStrLn "accepted client"
			forkIO $ do
				runClientActionT $ do
					client <- lift $ startClient handle hostName port :: ClientAction Client
					playerClient <- authenticateClient serverId client
					playerWorldClient <- sendClientWorld world playerClient
					lift $ putStrLn "got world client"
					actPlayerWorldClient playerWorldClient
					return ()
				return ()
		return server

stopServer :: Server -> IO ()
stopServer server = do
	putMVar (serverWait server) ()

serverRunning :: Server -> IO Bool
serverRunning Server {serverWait = wait} = do
	running <- tryTakeMVar wait
	
	case running of
		(Just x) -> do
			putMVar wait x
			return False
		Nothing -> return True

waitForServer :: Server -> IO ()
waitForServer Server {serverWait = wait} = do
	readMVar wait
	return ()