module Minecraft.Block
	( Block (..)
	, blockId
	, blockFromId
	, blockFromId_
	, blockSecondaryData
	) where
import Data.Word

data Block
	= Air
	| Stone
	| Grass
	| Dirt
	| Cobblestone
	| Wood
	| Sapling
	| Bedrock
	| Water {waterLevel :: Word8}
	| StationaryWater {stationaryWaterLevel :: Word8}
	| Lava {lavalLevel :: Word8}
	| StationaryLava {stationaryLavalLevel :: Word8}
	| Sand
	| Gravel
	| GoldOre
	| IronOre
	| CoalOre
	| Log
	| Leaves
	| Sponge
	| Glass
	| RedCloth
	| OrangeCloth
	| YellowCloth
	| LimeCloth
	| GreenCloth
	| AquaGreenCloth
	| CyanCloth
	| BlueCloth
	| PurpleCloth
	| IndigoCloth
	| VioletCloth
	| MagentaCloth
	| PinkCloth
	| BlackCloth
	| GrayCloth
	| WhiteCloth
	| YellowFlower
	| RedFlower
	| BrownMushroom
	| RedMushroom
	| GoldBlock
	| IronBlock
	| DoubleStep
	| Step
	| Brick
	| TNT
	| BookCase
	| MossyCobblestone
	| Obsidian
	| Torch {torchDirection :: Word8}
	| Fire
	| MobSpawner
	| WoodenStairs
	| Chest
	| RedstoneWire {redstoneCurrent :: Word8}
	| DiamondOre
	| DiamondBlock
	| Workbench
	| Crops {cropHeight :: Word8}
	| Soil {soilWetness :: Word8}
	| Furnace
	| BurningFurnace
	| SignPost
	| WoodenDoor
	| Ladder
	| MinecartRail {minecartRailOrientation :: Word8}
	| CobblestoneStairs
	| Sign
	| Lever
	| StonePressurePlate
	| IronDoor
	| WoodenPressurePlate
	| RedstoneOre
	| LightedRedstoneOre
	| RedstoneTorchOff
	| RedstoneTorchOn
	| StoneButton
	| Snow
	| Ice
	| SnowBlock
	| Cactus
	| Clay
	| Reed
	| Jukebox
	deriving (Eq, Show)

blockSecondaryData :: Integral a => Block -> a
blockSecondaryData (Water i)           = fromIntegral i
blockSecondaryData (StationaryWater i) = fromIntegral i
blockSecondaryData (Lava i)            = fromIntegral i
blockSecondaryData (StationaryLava i)  = fromIntegral i
blockSecondaryData (Soil i)            = fromIntegral i
blockSecondaryData (MinecartRail i)    = fromIntegral i
blockSecondaryData (Torch i)           = fromIntegral i
blockSecondaryData (RedstoneWire i)    = fromIntegral i
blockSecondaryData (Crops i)           = fromIntegral i
blockSecondaryData _                   = 0

blockId :: (Integral a) => Block -> a
blockId Air = 0
blockId Stone = 1
blockId Grass = 2
blockId Dirt = 3
blockId Cobblestone = 4
blockId Wood = 5
blockId Sapling = 6
blockId Bedrock = 7
blockId Water {} = 8
blockId StationaryWater {} = 9
blockId Lava {} = 10
blockId StationaryLava {} = 11
blockId Sand = 12
blockId Gravel = 13
blockId GoldOre = 14
blockId IronOre = 15
blockId CoalOre = 16
blockId Log = 17
blockId Leaves = 18
blockId Sponge = 19
blockId Glass = 20
blockId RedCloth = 21
blockId OrangeCloth = 22
blockId YellowCloth = 23
blockId LimeCloth = 24
blockId GreenCloth = 25
blockId AquaGreenCloth = 26
blockId CyanCloth = 27
blockId BlueCloth = 28
blockId PurpleCloth = 29
blockId IndigoCloth = 30
blockId VioletCloth = 31
blockId MagentaCloth = 32
blockId PinkCloth = 33
blockId BlackCloth = 34
blockId GrayCloth = 35
blockId WhiteCloth = 36
blockId YellowFlower = 37
blockId RedFlower = 38
blockId BrownMushroom = 39
blockId RedMushroom = 40
blockId GoldBlock = 41
blockId IronBlock = 42
blockId DoubleStep = 43
blockId Step = 44
blockId Brick = 45
blockId TNT = 46
blockId BookCase = 47
blockId MossyCobblestone = 48
blockId Obsidian = 49
blockId Torch {} = 50
blockId Fire = 51
blockId MobSpawner = 52
blockId WoodenStairs = 53
blockId Chest = 54
blockId RedstoneWire {} = 55
blockId DiamondOre = 56
blockId DiamondBlock = 57
blockId Workbench = 58
blockId Crops {} = 59
blockId Soil {} = 60
blockId Furnace = 61
blockId BurningFurnace = 62
blockId SignPost = 63
blockId WoodenDoor = 64
blockId Ladder = 65
blockId MinecartRail {} = 66
blockId CobblestoneStairs = 67
blockId Sign = 68
blockId Lever = 69
blockId StonePressurePlate = 70
blockId IronDoor = 71
blockId WoodenPressurePlate = 72
blockId RedstoneOre = 73
blockId LightedRedstoneOre = 74
blockId RedstoneTorchOff = 75
blockId RedstoneTorchOn = 76
blockId StoneButton = 77
blockId Snow = 78
blockId Ice = 79
blockId SnowBlock = 80
blockId Cactus = 81
blockId Clay = 82
blockId Reed = 83
blockId Jukebox = 84

blockFromId :: (Integral a, Integral b) => a -> b -> Block
blockFromId 0 _ = Air
blockFromId 1 _ = Stone
blockFromId 2 _ = Grass
blockFromId 3 _ = Dirt
blockFromId 4 _ = Cobblestone
blockFromId 5 _ = Wood
blockFromId 6 _ = Sapling
blockFromId 7 _ = Bedrock
blockFromId 8 i = Water (fromIntegral i)
blockFromId 9 i = StationaryWater (fromIntegral i)
blockFromId 10 i = Lava (fromIntegral i)
blockFromId 11 i = StationaryLava (fromIntegral i)
blockFromId 12 _ = Sand
blockFromId 13 _ = Gravel
blockFromId 14 _ = GoldOre
blockFromId 15 _ = IronOre
blockFromId 16 _ = CoalOre
blockFromId 17 _ = Log
blockFromId 18 _ = Leaves
blockFromId 19 _ = Sponge
blockFromId 20 _ = Glass
blockFromId 21 _ = RedCloth
blockFromId 22 _ = OrangeCloth
blockFromId 23 _ = YellowCloth
blockFromId 24 _ = LimeCloth
blockFromId 25 _ = GreenCloth
blockFromId 26 _ = AquaGreenCloth
blockFromId 27 _ = CyanCloth
blockFromId 28 _ = BlueCloth
blockFromId 29 _ = PurpleCloth
blockFromId 30 _ = IndigoCloth
blockFromId 31 _ = VioletCloth
blockFromId 32 _ = MagentaCloth
blockFromId 33 _ = PinkCloth
blockFromId 34 _ = BlackCloth
blockFromId 35 _ = GrayCloth
blockFromId 36 _ = WhiteCloth
blockFromId 37 _ = YellowFlower
blockFromId 38 _ = RedFlower
blockFromId 39 _ = BrownMushroom
blockFromId 40 _ = RedMushroom
blockFromId 41 _ = GoldBlock
blockFromId 42 _ = IronBlock
blockFromId 43 _ = DoubleStep
blockFromId 44 _ = Step
blockFromId 45 _ = Brick
blockFromId 46 _ = TNT
blockFromId 47 _ = BookCase
blockFromId 48 _ = MossyCobblestone
blockFromId 49 _ = Obsidian
blockFromId 50 i = Torch (fromIntegral i)
blockFromId 51 _ = Fire
blockFromId 52 _ = MobSpawner
blockFromId 53 _ = WoodenStairs
blockFromId 54 _ = Chest
blockFromId 55 i = RedstoneWire (fromIntegral i)
blockFromId 56 _ = DiamondOre
blockFromId 57 _ = DiamondBlock
blockFromId 58 _ = Workbench
blockFromId 59 i = Crops (fromIntegral i)
blockFromId 60 i = Soil (fromIntegral i)
blockFromId 61 _ = Furnace
blockFromId 62 _ = BurningFurnace
blockFromId 63 _ = SignPost
blockFromId 64 _ = WoodenDoor
blockFromId 65 _ = Ladder
blockFromId 66 i = MinecartRail (fromIntegral i)
blockFromId 67 _ = CobblestoneStairs
blockFromId 68 _ = Sign
blockFromId 69 _ = Lever
blockFromId 70 _ = StonePressurePlate
blockFromId 71 _ = IronDoor
blockFromId 72 _ = WoodenPressurePlate
blockFromId 73 _ = RedstoneOre
blockFromId 74 _ = LightedRedstoneOre
blockFromId 75 _ = RedstoneTorchOff
blockFromId 76 _ = RedstoneTorchOn
blockFromId 77 _ = StoneButton
blockFromId 78 _ = Snow
blockFromId 79 _ = Ice
blockFromId 80 _ = SnowBlock
blockFromId 81 _ = Cactus
blockFromId 82 _ = Clay
blockFromId 83 _ = Reed
blockFromId 84 _ = Jukebox

blockFromId_ :: Integral a => a -> Block
blockFromId_ = flip blockFromId 0

instance Enum Block where
	toEnum = blockFromId_
	fromEnum = blockId