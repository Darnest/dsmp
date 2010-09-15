module Minecraft.Inventory
	( InventoryItemSlot (..)
	, Inventory
	, inventoryBounds
	, inventoryMinBound
	, inventoryMaxBound
	, inventory
	, emptyInventory
	, getInventorySlot
	, clearInventory
	, setInventorySlot
	, clearInventorySlot
	, inventorySlots
	, inventorySlotCount
	) where
import Minecraft.Item (Item)

import Data.Array (Array)
import Data.Word
import qualified Data.Array as Array

data InventoryItemSlot
	= InventoryItemSlotItem
		{ inventoryItemSlotItem :: Item
		, inventoryItemSlotCount :: Word8
		}
	| InventoryItemSlotEmpty
	deriving (Show, Eq)

newtype Inventory = Inventory (Array Word InventoryItemSlot)
	deriving (Show, Eq)

inventoryBounds :: Integral a => Inventory -> (a, a)
inventoryBounds (Inventory a) = let bounds = Array.bounds a in (fromIntegral $ fst bounds, fromIntegral $ snd bounds)

inventoryMinBound :: Integral a => Inventory -> a
inventoryMinBound (Inventory a) = fromIntegral $ fst $ Array.bounds a

inventoryMaxBound :: Integral a => Inventory -> a
inventoryMaxBound (Inventory a) = fromIntegral $ snd $ Array.bounds a

inventory :: (Integral a, Array.Ix a) => a -> [InventoryItemSlot] -> Inventory
inventory size elems = Inventory $ Array.listArray (0, fromIntegral size - 1) elems

emptyInventory :: Integral a => a -> Inventory
emptyInventory size = Inventory $ Array.listArray (0, fromIntegral size - 1) (repeat InventoryItemSlotEmpty)

getInventorySlot :: Integral a => Inventory -> a -> InventoryItemSlot
getInventorySlot (Inventory a) i = a Array.! (fromIntegral i)

clearInventory :: Inventory -> Inventory
clearInventory (Inventory a) = Inventory $ fmap (\x -> InventoryItemSlotEmpty) a

setInventorySlot :: Integral a => Inventory -> a -> InventoryItemSlot -> Inventory
setInventorySlot (Inventory a) i slot = Inventory $ a Array.// [(fromIntegral i, slot)]

clearInventorySlot :: Integral a => Inventory -> a -> Inventory
clearInventorySlot (Inventory a) i = Inventory $ a Array.// [(fromIntegral i, InventoryItemSlotEmpty)]

inventorySlots :: Inventory -> [InventoryItemSlot]
inventorySlots (Inventory a) = Array.elems a

inventorySlotCount :: Integral a => Inventory -> a
inventorySlotCount inventory = (inventoryMaxBound inventory) + 1