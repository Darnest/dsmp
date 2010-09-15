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
import qualified Minecraft.Block as Block
import Minecraft.Server.ServerId
import Minecraft.Player.Inventory
import Minecraft.Network.Protocol.ServerPacket.Encoding
import Minecraft.Time
import Minecraft.Server.MapChunkVectorTree

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
import Data.Int
import Data.Vector.V3


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
		, playerWorldClientPlayerEntity :: MVar PlayerEntity
		, playerWorldClientWorld :: World
		, playerWorldClientLoadedChunks :: MVar MapChunkVectorTree
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
		succ <- lift $ writeSChan wChan packet
		--lift $ putStrLn ("sent packet to wchan " ++ (take 1000 $ show packet))
		if succ then
				return ()
			else do
				disconnect <- lift $ readMVar dAction
				failedClientActionResultT disconnect
	getPacket client@Client
		{ clientPacketReadChan = rChan
		, clientDisconnectAction = dAction
		} = do
		mPacket <- lift $ readSChan rChan
		return ()
		case mPacket of
			(Just packet) -> do
				--lift $ putStrLn $ "got packet from rchan: " ++ (show packet)
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
			lift $ putStrLn "stopping client..."
			if succ then do
					lift $ do
						forkIO $ do
							putStrLn "closing wchan..."
							closeSChan wChan
							putStrLn "closed wchan..."
							return ()
						forkIO $ do
							putStrLn "closing rchan..."
							closeSChan rChan
							putStrLn "closed rchan..."
							return ()
					failedClientActionResultT $ action
				else do
					lift $ putStrLn "failed to stop client"
					disconnect <- lift $ readMVar mAction
					failedClientActionResultT disconnect

	kickClient client s = do
		sendPacket client $ ServerPacket.Kick s
		stopClient client $ ClientKicked s
	
unexpectedDisconnectClient :: ClientBehavior client => client -> String -> ClientAction a
unexpectedDisconnectClient client s = stopClient client $ UnexpectedClientDisconnect s

sendPackets :: ClientBehavior client => client -> [ServerPacket] -> ClientAction ()
sendPackets client = mapM_ (sendPacket client)

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
			, playerInventory = emptyPlayerInventory
			, playerDigStatus = PlayerNotDigging
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
			{ ServerPacket.mapChunkX = x * 16
			, ServerPacket.mapChunkY = 0
			, ServerPacket.mapChunkZ = z * 16
			, ServerPacket.mapChunkXSize = 15
			, ServerPacket.mapChunkYSize = 127
			, ServerPacket.mapChunkZSize = 15
			, ServerPacket.mapChunkBlockTypes = map blockId blocks
			, ServerPacket.mapChunkBlockMetadata = map blockSecondaryData blocks
			, ServerPacket.mapChunkBlockLighting = map mapBlockLighting chunk
			}

sendUnloadClientMapChunk :: PlayerWorldClient -> MapChunkVector -> ClientAction ()
sendUnloadClientMapChunk client v = do
		let
			x = mapChunkVectorX v
			z = mapChunkVectorZ v
		
		sendPacket client ServerPacket.PreChunk
			{ ServerPacket.preChunkX = x
			, ServerPacket.preChunkZ = z
			, ServerPacket.preChunkMode = False
			}

mapChunkRadius :: Integral a => a
mapChunkRadius = 5

sendAllClientMapChunks :: PlayerWorldClient -> MapChunkVector -> ClientAction ()
sendAllClientMapChunks
	client@PlayerWorldClient
		{ playerWorldClientLoadedChunks = mLoadedChunks
		}
	chunkVector = do
		loadedChunks <- lift $ takeMVar mLoadedChunks
		chunks <- doSend loadedChunks minX minZ
		lift $ putMVar mLoadedChunks chunks
	where
		aChunkRadius = mapChunkRadius - 1
		xi = mapChunkVectorX chunkVector
		zi = mapChunkVectorZ chunkVector
		minX = xi - mapChunkRadius
		minZ = zi - mapChunkRadius
		maxX = xi + aChunkRadius
		maxZ = zi + aChunkRadius
		doSend :: MapChunkVectorTree -> Int32 -> Int32 -> ClientAction MapChunkVectorTree
		doSend tree x z = do
			newTree <- case mapChunkVector x z of
				(Just v) -> do
					sendMapChunk client v
					return (mapChunkVectorTreeInsert tree v)
				Nothing -> return tree
			if x < maxX then
					doSend newTree (x + 1) z
				else
					if z < maxZ then
							doSend newTree minX (z + 1)
						else
							return newTree

updateClientMapChunks :: PlayerWorldClient -> MapChunkVector -> ClientAction ()
updateClientMapChunks
	client@PlayerWorldClient
		{ playerWorldClientLoadedChunks = mLoadedChunks
		}
	chunkVector = do
		loadedChunks <- lift $ takeMVar mLoadedChunks
		chunks <- doUnload loadedChunks
		chunks2 <- doSend chunks minX minZ
		lift $ putMVar mLoadedChunks chunks2
		where
			aChunkRadius = mapChunkRadius - 1
			xi = mapChunkVectorX chunkVector
			zi = mapChunkVectorZ chunkVector
			minX = xi - mapChunkRadius
			minZ = zi - mapChunkRadius
			maxX = xi + aChunkRadius
			maxZ = zi + aChunkRadius
			doUnload :: MapChunkVectorTree -> ClientAction MapChunkVectorTree
			doUnload tree = foldM (\tree v -> do
				let
					x = mapChunkVectorX v
					z = mapChunkVectorZ v
					in if x < minX || x > maxX || z < minZ || z > maxZ then do
							sendUnloadClientMapChunk client v
							return (mapChunkVectorTreeRemove tree v)
						else
							return tree
				) tree (mapChunkVectorTreeList tree)
			doSend :: MapChunkVectorTree -> Int32 -> Int32 -> ClientAction MapChunkVectorTree
			doSend tree x z = do
				newTree <- case mapChunkVector x z of
					(Just v) -> if mapChunkVectorTreeNotMember tree v then do
							sendMapChunk client v
							return (mapChunkVectorTreeInsert tree v)
						else
							return tree
					Nothing -> return tree
				if x < maxX then
						doSend newTree (x + 1) z
					else
						if z < maxZ then
								doSend newTree minX (z + 1)
							else
								return newTree

sendClientWorld :: World -> PlayerClient -> ClientAction PlayerWorldClient
sendClientWorld world@World
	{ worldMap = map
	} PlayerClient
	{ playerClientPlayer = player
	, playerClientClient = client
	} = do
		playerEntity <- lift $ addWorldPlayer world player
		mPlayerEntity <- lift $ newMVar playerEntity
		mLoadedChunks <- lift $ newMVar emptyMapChunkVectorTree
		let playerWorldClient = PlayerWorldClient
			{ playerWorldClientPlayer = player
			, playerWorldClientPlayerEntity = mPlayerEntity
			, playerWorldClientClient = client
			, playerWorldClientWorld = world
			, playerWorldClientLoadedChunks = mLoadedChunks
			}
		let blockVector = entityMapBlockVector playerEntity
		sendPackets client (encodeSetPlayerInventory $ playerInventory player)
		sendPacket client $ encodePlayerBlockSpawn $ entityMapBlockVector playerEntity
		sendAllClientMapChunks playerWorldClient (entityMapChunkVector playerEntity)
		sendPacket client $ encodePlayerPositionLook playerEntity
		sendPacket client $ encodeMinecraftTime minecraftTimeNoon
		return playerWorldClient

decodeAngleFloat = (/ 180) . realToFrac

actPlayerWorldClient :: PlayerWorldClient -> ClientAction ()
actPlayerWorldClient client@PlayerWorldClient
	{ playerWorldClientWorld = world
	, playerWorldClientPlayerEntity = mPlayerEntity
	} = do
		playerEntity <- lift $ takeMVar mPlayerEntity
		act playerEntity
		where act playerEntity = do
			packet <- getNonPingPacket client
			let map = worldMap world
			maybePlayerEntity <- case packet of
				ClientPacket.PlayerPosition
					{ ClientPacket.playerPositionX = x
					, ClientPacket.playerPositionY = y
					, ClientPacket.playerPositionStance = stance
					, ClientPacket.playerPositionZ = z
					, ClientPacket.playerPositionOnGround = onGround
					} -> do
						newPlayerEntity <- lift $ moveWorldEntity world playerEntity
							(entityPosition playerEntity)
								{ entityPositionVector = Vector3
									{ v3x = x
									, v3y = y
									, v3z = z
									}
								}
						return $ Just newPlayerEntity
				ClientPacket.PlayerLook
					{ ClientPacket.playerLookRotation = rot
					, ClientPacket.playerLookPitch = pitch
					, ClientPacket.playerLookOnGround = onGRound
					} -> do
						newPlayerEntity <- lift $ moveWorldEntity world playerEntity
							(entityPosition playerEntity)
								{ entityPositionRotation = decodeAngleFloat rot
								, entityPositionPitch = decodeAngleFloat pitch
								}
						return $ Just newPlayerEntity
				ClientPacket.PlayerPositionLook
					{ ClientPacket.playerPositionLookX = x
					, ClientPacket.playerPositionLookY = y
					, ClientPacket.playerPositionLookStance = stance
					, ClientPacket.playerPositionLookZ = z
					, ClientPacket.playerPositionLookRotation = rot
					, ClientPacket.playerPositionLookPitch = pitch
					, ClientPacket.playerPositionLookOnGround = onGround
					} -> do
						newPlayerEntity <- lift $ moveWorldEntity world playerEntity
							(entityPosition playerEntity)
								{ entityPositionRotation = decodeAngleFloat rot
								, entityPositionPitch = decodeAngleFloat pitch
								, entityPositionVector = Vector3
									{ v3x = x
									, v3y = y
									, v3z = z
									}
								}
						return $ Just newPlayerEntity
				ClientPacket.Dig
					{ ClientPacket.digStatus = status
					, ClientPacket.digX = x
					, ClientPacket.digY = y
					, ClientPacket.digZ = z
					, ClientPacket.digBlockDirection = direction
					} -> do
						case (mapBlockVector x (fromIntegral y) z) of
							(Just blockVector) ->
								case status of
									_ -> do
										let block = Block.Air
										lift $ setMapBlock map blockVector MapBlock
											{ mapBlockBlock = block
											, mapBlockLighting = 0x0F
											}
										sendPacket client $ encodeBlockChange blockVector block
										return Nothing
							Nothing -> return Nothing
				_ -> return Nothing
			nextPlayerEntity <- case maybePlayerEntity of
				(Just newPlayerEntity) -> do
					updateClientMapChunks client (entityMapChunkVector newPlayerEntity)
					return newPlayerEntity
				Nothing -> return playerEntity
			act nextPlayerEntity
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