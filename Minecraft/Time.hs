module Minecraft.Time
	( MinecraftTime
	, minecraftTime
	, minecraftTimeMinutes
	, minecraftTimeFromMinutes
	, minecraftTimeNoon
	) where

newtype MinecraftTime
	= MinecraftTimeMinutes
		{ rminecraftTimeMinutes :: Int
		}

minutesInDay :: Integral a => a
minutesInDay = 24 * 60

minecraftTime :: (Integral a, Integral b) => a -> b -> MinecraftTime
minecraftTime hours minutes =
	MinecraftTimeMinutes $ (((fromIntegral (hours `rem` 24)) * 60) + (fromIntegral (minutes `rem` minutesInDay))) `rem` minutesInDay

minecraftTimeFromMinutes :: Integral a => a -> MinecraftTime
minecraftTimeFromMinutes minutes = MinecraftTimeMinutes (fromIntegral (minutes `rem` minutesInDay))

minecraftTimeMinutes :: Integral a => MinecraftTime -> a
minecraftTimeMinutes = fromIntegral . rminecraftTimeMinutes

minecraftTimeNoon = minecraftTime 12 0