module Minecraft.Item
	( Item (..)
	, itemId
	, itemFromId
	, itemFromId_
	, itemLife
	) where
import Data.Word
import Minecraft.Block (Block, blockId, blockFromId_)

data Item
	= BlockItem Block
	| IronShovel {itemLifeR :: Word8}
	| IronPickaxe {itemLifeR :: Word8}
	| IronAxe {itemLifeR :: Word8}
	| FlintAndSteel {itemLifeR :: Word8}
	| Apple
	| Bow {itemLifeR :: Word8}
	| Arrow
	| Coal
	| Diamond
	| IronIngot
	| GoldIngot
	| IronSword {itemLifeR :: Word8}
	| WoodenSword {itemLifeR :: Word8}
	| WoodenShovel {itemLifeR :: Word8}
	| WoodenPickaxe {itemLifeR :: Word8}
	| WoodenAxe {itemLifeR :: Word8}
	| StoneSword {itemLifeR :: Word8}
	| StoneShovel {itemLifeR :: Word8}
	| StonePickaxe {itemLifeR :: Word8}
	| StoneAxe {itemLifeR :: Word8}
	| DiamondSword {itemLifeR :: Word8}
	| DiamondShovel {itemLifeR :: Word8}
	| DiamondPickaxe {itemLifeR :: Word8}
	| DiamondAxe {itemLifeR :: Word8}
	| Stick
	| Bowl
	| MushroomSoup
	| GoldSword {itemLifeR :: Word8}
	| GoldShovel {itemLifeR :: Word8}
	| GoldPickaxe {itemLifeR :: Word8}
	| GoldAxe {itemLifeR :: Word8}
	| String
	| Feather
	| Gunpowder
	| WoodenHoe {itemLifeR :: Word8}
	| StoneHoe {itemLifeR :: Word8}
	| IronHoe {itemLifeR :: Word8}
	| DiamondHoe {itemLifeR :: Word8}
	| GoldHoe {itemLifeR :: Word8}
	| Seeds
	| Wheat
	| Bread
	| LeatherHelmet {itemLifeR :: Word8}
	| LeatherChestplate {itemLifeR :: Word8}
	| LeatherPants {itemLifeR :: Word8}
	| LeatherBoots {itemLifeR :: Word8}
	| ChainmailHelmet {itemLifeR :: Word8}
	| ChainmailChestplate {itemLifeR :: Word8}
	| ChainmailPants {itemLifeR :: Word8}
	| ChainmailBoots {itemLifeR :: Word8}
	| IronHelmet {itemLifeR :: Word8}
	| IronChestplate {itemLifeR :: Word8}
	| IronPants {itemLifeR :: Word8}
	| IronBoots {itemLifeR :: Word8}
	| DiamondHelmet {itemLifeR :: Word8}
	| DiamondChestplate {itemLifeR :: Word8}
	| DiamondPants {itemLifeR :: Word8}
	| DiamondBoots {itemLifeR :: Word8}
	| GoldHelmet {itemLifeR :: Word8}
	| GoldChestplate {itemLifeR :: Word8}
	| GoldPants {itemLifeR :: Word8}
	| GoldBoots {itemLifeR :: Word8}
	| Flint
	| Pork
	| GrilledPork
	| Paintings
	| GoldenApple
	| Sign
	| WoodenDoor
	| Bucket
	| WaterBucket
	| LavaBucket
	| MineCart
	| Saddle
	| IronDoor
	| RedStone
	| SnowBall
	| Boat
	| Leather
	| MilkBucket
	| ClayBrick
	| ClayBalls
	| Reed
	| Paper
	| Book
	| SlimeBall
	| StorageMinecart
	| PoweredMinecart
	| Egg
	| GoldRecord
	| GreenRecord
	deriving (Eq, Show)

itemId :: Integral a => Item -> a
itemId (BlockItem b) = blockId b
itemId IronShovel {} = 256
itemId IronPickaxe {} = 257
itemId IronAxe {} = 258
itemId FlintAndSteel {} = 259
itemId Apple {} = 260
itemId Bow {} = 261
itemId Arrow {} = 262
itemId Coal {} = 263
itemId Diamond {} = 264
itemId IronIngot {} = 265
itemId GoldIngot {} = 266
itemId IronSword {} = 267
itemId WoodenSword {} = 268
itemId WoodenShovel {} = 269
itemId WoodenPickaxe {} = 270
itemId WoodenAxe {} = 271
itemId StoneSword {} = 272
itemId StoneShovel {} = 273
itemId StonePickaxe {} = 274
itemId StoneAxe {} = 275
itemId DiamondSword {} = 276
itemId DiamondShovel {} = 277
itemId DiamondPickaxe {} = 278
itemId DiamondAxe {} = 279
itemId Stick {} = 280
itemId Bowl {} = 281
itemId MushroomSoup {} = 282
itemId GoldSword {} = 283
itemId GoldShovel {} = 284
itemId GoldPickaxe {} = 285
itemId GoldAxe {} = 286
itemId String {} = 287
itemId Feather {} = 288
itemId Gunpowder {} = 289
itemId WoodenHoe {} = 290
itemId StoneHoe {} = 291
itemId IronHoe {} = 292
itemId DiamondHoe {} = 293
itemId GoldHoe {} = 294
itemId Seeds {} = 295
itemId Wheat {} = 296
itemId Bread {} = 297
itemId LeatherHelmet {} = 298
itemId LeatherChestplate {} = 299
itemId LeatherPants {} = 300
itemId LeatherBoots {} = 301
itemId ChainmailHelmet {} = 302
itemId ChainmailChestplate {} = 303
itemId ChainmailPants {} = 304
itemId ChainmailBoots {} = 305
itemId IronHelmet {} = 306
itemId IronChestplate {} = 307
itemId IronPants {} = 308
itemId IronBoots {} = 309
itemId DiamondHelmet {} = 310
itemId DiamondChestplate {} = 311
itemId DiamondPants {} = 312
itemId DiamondBoots {} = 313
itemId GoldHelmet {} = 314
itemId GoldChestplate {} = 315
itemId GoldPants {} = 316
itemId GoldBoots {} = 317
itemId Flint {} = 318
itemId Pork {} = 319
itemId GrilledPork {} = 320
itemId Paintings {} = 321
itemId GoldenApple {} = 322
itemId Sign {} = 323
itemId WoodenDoor {} = 324
itemId Bucket {} = 325
itemId WaterBucket {} = 326
itemId LavaBucket {} = 327
itemId MineCart {} = 328
itemId Saddle {} = 329
itemId IronDoor {} = 330
itemId RedStone {} = 331
itemId SnowBall {} = 332
itemId Boat {} = 333
itemId Leather {} = 334
itemId MilkBucket {} = 335
itemId ClayBrick {} = 336
itemId ClayBalls {} = 337
itemId Reed {} = 338
itemId Paper {} = 339
itemId Book {} = 340
itemId SlimeBall {} = 341
itemId StorageMinecart {} = 342
itemId PoweredMinecart {} = 343
itemId Egg {} = 344
itemId GoldRecord {} = 2256
itemId GreenRecord {} = 2257

itemFromId :: (Integral a, Integral b) => a -> b -> Item
itemFromId i _ | i <= 255 = BlockItem (blockFromId_ i)
itemFromId 256 life = IronShovel (fromIntegral life)
itemFromId 257 life = IronPickaxe (fromIntegral life)
itemFromId 258 life = IronAxe (fromIntegral life)
itemFromId 259 life = FlintAndSteel (fromIntegral life)
itemFromId 260 _ = Apple
itemFromId 261 life = Bow (fromIntegral life)
itemFromId 262 _ = Arrow
itemFromId 263 _ = Coal
itemFromId 264 _ = Diamond
itemFromId 265 _ = IronIngot 
itemFromId 266 _ = GoldIngot 
itemFromId 267 life = IronSword (fromIntegral life)
itemFromId 268 life = WoodenSword (fromIntegral life)
itemFromId 269 life = WoodenShovel (fromIntegral life)
itemFromId 270 life = WoodenPickaxe (fromIntegral life)
itemFromId 271 life = WoodenAxe (fromIntegral life)
itemFromId 272 life = StoneSword (fromIntegral life)
itemFromId 273 life = StoneShovel (fromIntegral life)
itemFromId 274 life = StonePickaxe (fromIntegral life)
itemFromId 275 life = StoneAxe (fromIntegral life)
itemFromId 276 life = DiamondSword (fromIntegral life)
itemFromId 277 life = DiamondShovel (fromIntegral life)
itemFromId 278 life = DiamondPickaxe (fromIntegral life)
itemFromId 279 life = DiamondAxe (fromIntegral life)
itemFromId 280 _ = Stick
itemFromId 281 _ = Bowl
itemFromId 282 _ = MushroomSoup
itemFromId 283 life = GoldSword (fromIntegral life)
itemFromId 284 life = GoldShovel (fromIntegral life)
itemFromId 285 life = GoldPickaxe (fromIntegral life)
itemFromId 286 life = GoldAxe (fromIntegral life)
itemFromId 287 _ = String
itemFromId 288 _ = Feather
itemFromId 289 _ = Gunpowder
itemFromId 290 life = WoodenHoe (fromIntegral life)
itemFromId 291 life = StoneHoe (fromIntegral life)
itemFromId 292 life = IronHoe (fromIntegral life)
itemFromId 293 life = DiamondHoe (fromIntegral life)
itemFromId 294 life = GoldHoe (fromIntegral life)
itemFromId 295 _ = Seeds
itemFromId 296 _ = Wheat
itemFromId 297 _ = Bread
itemFromId 298 life = LeatherHelmet (fromIntegral life)
itemFromId 299 life = LeatherChestplate (fromIntegral life)
itemFromId 300 life = LeatherPants (fromIntegral life)
itemFromId 301 life = LeatherBoots (fromIntegral life)
itemFromId 302 life = ChainmailHelmet (fromIntegral life)
itemFromId 303 life = ChainmailChestplate (fromIntegral life)
itemFromId 304 life = ChainmailPants (fromIntegral life)
itemFromId 305 life = ChainmailBoots (fromIntegral life)
itemFromId 306 life = IronHelmet (fromIntegral life)
itemFromId 307 life = IronChestplate (fromIntegral life)
itemFromId 308 life = IronPants (fromIntegral life)
itemFromId 309 life = IronBoots (fromIntegral life)
itemFromId 310 life = DiamondHelmet (fromIntegral life)
itemFromId 311 life = DiamondChestplate (fromIntegral life)
itemFromId 312 life = DiamondPants (fromIntegral life)
itemFromId 313 life = DiamondBoots (fromIntegral life)
itemFromId 314 life = GoldHelmet (fromIntegral life)
itemFromId 315 life = GoldChestplate (fromIntegral life)
itemFromId 316 life = GoldPants (fromIntegral life)
itemFromId 317 life = GoldBoots (fromIntegral life)
itemFromId 318 _ = Flint
itemFromId 319 _ = Pork
itemFromId 320 _ = GrilledPork
itemFromId 321 _ = Paintings
itemFromId 322 _ = GoldenApple
itemFromId 323 _ = Sign
itemFromId 324 _ = WoodenDoor
itemFromId 325 _ = Bucket
itemFromId 326 _ = WaterBucket
itemFromId 327 _ = LavaBucket
itemFromId 328 _ = MineCart
itemFromId 329 _ = Saddle
itemFromId 330 _ = IronDoor
itemFromId 331 _ = RedStone
itemFromId 332 _ = SnowBall
itemFromId 333 _ = Boat
itemFromId 334 _ = Leather
itemFromId 335 _ = MilkBucket
itemFromId 336 _ = ClayBrick
itemFromId 337 _ = ClayBalls
itemFromId 338 _ = Reed
itemFromId 339 _ = Paper
itemFromId 340 _ = Book
itemFromId 341 _ = SlimeBall
itemFromId 342 _ = StorageMinecart
itemFromId 343 _ = PoweredMinecart
itemFromId 344 _ = Egg
itemFromId 2256 _ = GoldRecord
itemFromId 2257 _ = GreenRecord

itemFromId_ :: Integral a => a -> Item
itemFromId_ = flip itemFromId 0

itemLife :: Integral a => Item -> a
itemLife BlockItem {} = 0
itemLife Apple = 0
itemLife Arrow = 0
itemLife Coal = 0
itemLife Diamond = 0
itemLife IronIngot = 0
itemLife GoldIngot = 0
itemLife Stick = 0
itemLife Bowl = 0
itemLife MushroomSoup = 0
itemLife String = 0
itemLife Feather = 0
itemLife Gunpowder = 0
itemLife Seeds = 0
itemLife Wheat = 0
itemLife Bread = 0
itemLife Flint = 0
itemLife Pork = 0
itemLife GrilledPork = 0
itemLife Paintings = 0
itemLife GoldenApple = 0
itemLife Sign = 0
itemLife WoodenDoor = 0
itemLife Bucket = 0
itemLife WaterBucket = 0
itemLife LavaBucket = 0
itemLife MineCart = 0
itemLife Saddle = 0
itemLife IronDoor = 0
itemLife RedStone = 0
itemLife SnowBall = 0
itemLife Boat = 0
itemLife Leather = 0
itemLife MilkBucket = 0
itemLife ClayBrick = 0
itemLife ClayBalls = 0
itemLife Reed = 0
itemLife Paper = 0
itemLife Book = 0
itemLife SlimeBall = 0
itemLife StorageMinecart = 0
itemLife PoweredMinecart = 0
itemLife Egg = 0
itemLife GoldRecord = 0
itemLife GreenRecord = 0
itemLife x = fromIntegral (itemLifeR x)

instance Enum Item where
	toEnum = itemFromId_
	fromEnum = itemId