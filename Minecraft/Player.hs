module Minecraft.Player
	( Player (..)
	) where
import Data.ByteString as BS
import Minecraft.Item

data Player
	= Player
		{ playerUsername :: String
		, playerHeldItem :: Maybe Item
		}
	deriving (Show, Eq)