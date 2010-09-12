module Main
	( main
	) where
import Minecraft.Network.Server
import Network (PortID (..))
import Control.Monad

main = do
	putStrLn "starting server..."
	server <- startServer ServerConfig
		{ serverConfigPort = (PortNumber 25565)
		}
	putStrLn "server started"
	--Workaround for haskell deadlock detection until there is an actual interface
	forever $ getLine
	return ()
	--waitForServer server