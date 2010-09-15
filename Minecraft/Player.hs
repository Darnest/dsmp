module Minecraft.Player
	( Player (..)
	, PlayerDigStatus (..)
	) where
import Minecraft.Item
import Minecraft.Player.Inventory
import Minecraft.Map
import Minecraft.Block (Block)

import Data.Time.Clock
import Data.ByteString as BS

data Player
	= Player
		{ playerUsername :: String
		, playerHeldItem :: Maybe Item
		, playerInventory :: PlayerInventory
		, playerDigStatus :: PlayerDigStatus
		}
	deriving (Eq)

data PlayerDigStatus
	= PlayerNotDigging
	| PlayerDigging
		{ playerDiggingPlace :: MapBlockVector
		, playerDiggingBlock :: Block
		, playerDiggingStartTime :: UTCTime
		}
	deriving (Eq)