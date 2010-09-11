module Minecraft.Network.Minecraft_Net
	( verifyUsername
	) where
import qualified Data.ByteString as BS
import qualified Network.HTTP as HTTP
import Network.URI
import Data.Maybe

unsafeParseURI = fromJust . parseURIReference

gameURI = unsafeParseURI "/game/"

checkServerURI = fromJust $ (unsafeParseURI "checkserver.jsp") `relativeTo` gameURI

buildCheckServerURI :: String -> String -> URI
buildCheckServerURI user serverId
	=  checkServerURI
		{ uriQuery
			=  "?user=" ++ (HTTP.urlEncode user)
			++ "&serverId=" ++ (HTTP.urlEncode serverId)
		}

joinServerURI = fromJust $ (unsafeParseURI "joinserver.jsp") `relativeTo` gameURI

buildJoinServerURI :: String -> String -> String -> URI
buildJoinServerURI user sessionId serverId
	=  checkServerURI
		{ uriQuery
			= "?user=" ++ (HTTP.urlEncode user)
			++ "&sessionId=" ++ (HTTP.urlEncode sessionId)
			++ "&serverId=" ++ (HTTP.urlEncode serverId)
		}

getVersionURI = fromJust $ (unsafeParseURI "getversion.jsp") `relativeTo` gameURI

buildGetVersionURI :: String -> String -> String -> URI
buildGetVersionURI user password version
	=  checkServerURI
		{ uriQuery
			= "?user=" ++ (HTTP.urlEncode user)
			++ "&password=" ++ (HTTP.urlEncode password)
			++ "&version=" ++ (HTTP.urlEncode version)
		}

minecraftRequest :: HTTP.Request String
minecraftRequest = HTTP.Request
	{ HTTP.rqHeaders = [HTTP.Header HTTP.HdrHost "www.minecraft.net"]
	, HTTP.rqMethod = undefined
	, HTTP.rqBody = undefined
	, HTTP.rqURI = undefined
	}

minecraftGETRequest :: HTTP.Request String
minecraftGETRequest = HTTP.Request
	{ HTTP.rqHeaders = [HTTP.Header HTTP.HdrHost "www.minecraft.net"]
	, HTTP.rqMethod = HTTP.GET
	, HTTP.rqBody = ""
	, HTTP.rqURI = undefined
	}

--serverJoinSessionId :: String -> 

verifyUsername :: String -> String -> IO (Maybe Bool)
verifyUsername serverId username = do
	let uri = buildCheckServerURI username serverId
	res <- HTTP.simpleHTTP minecraftGETRequest {HTTP.rqURI = uri}
	case res of
		Left _ -> return Nothing
		Right HTTP.Response
			{ HTTP.rspCode = code
			, HTTP.rspBody = body
			}  -> case code of
				(2, _, _) -> case body of
					"YES" -> return $ Just True
					_     -> return $ Just False
				_         -> return Nothing