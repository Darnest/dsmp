module Minecraft.Player.Inventory
	( PlayerInventory (..)
	, newPlayerInventory
	, emptyPlayerInventory
	) where
import Minecraft.Inventory

data PlayerInventory = PlayerInventory
	{ playerInventoryMain :: Inventory
	, playerInventoryEquipped :: Inventory
	, playerInventoryCrafting :: Inventory
	}
	deriving (Show, Eq)

newPlayerInventory :: [InventoryItemSlot] -> [InventoryItemSlot] -> [InventoryItemSlot] -> PlayerInventory
newPlayerInventory main equipped crafting = PlayerInventory
	{ playerInventoryMain = inventory 36 main
	, playerInventoryEquipped = inventory 4 equipped
	, playerInventoryCrafting = inventory 4 crafting
	}

emptyPlayerInventory = PlayerInventory
	{ playerInventoryMain = emptyInventory 36
	, playerInventoryEquipped = emptyInventory 4
	, playerInventoryCrafting = emptyInventory 4
	}