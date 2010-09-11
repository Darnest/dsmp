module Minecraft.Object
	( Object (..)
	, objectId
	, objectFromId
	) where

data Object = Minecart | StorageCart | PoweredCart | Boat
	deriving (Eq, Show)

objectId :: Integral a => Object -> a
objectId Minecart = 10
objectId StorageCart = 11
objectId PoweredCart = 12
objectId Boat = 1

objectFromId :: Integral a => a -> Maybe Object
objectFromId 10 = Just Minecart
objectFromId 11 = Just StorageCart
objectFromId 12 = Just PoweredCart
objectFromId 1 = Just Boat
objectFromId _ = Nothing