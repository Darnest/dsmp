module Minecraft.Mob
	( Mob (..)
	, mobId
	) where

data Mob = Mob
	deriving (Show, Eq)

mobId :: Integral a => Mob -> a
mobId mob = 0