module Minecraft.Entity.EntityId
	( EntityIdGenerator
	, newEntityIdGenerator
	, generateEntityId
	, recycleEntityId
	, availableEntityIds
	) where
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map

data EntityIdGenerator = EntityIdGenerator (Map Word32 ()) Word32
	deriving (Show, Eq)

generateEntityId :: EntityIdGenerator -> Maybe (EntityIdGenerator, Word32)
generateEntityId (EntityIdGenerator map 4294967295)
	| Map.null map = Nothing
	| otherwise = (\((k, _), newMap) -> Just (EntityIdGenerator newMap 0, k)) $ Map.deleteFindMin map
	
generateEntityId (EntityIdGenerator map i) = Just (EntityIdGenerator map (i + 1), i)

recycleEntityId :: EntityIdGenerator -> Word32 -> EntityIdGenerator
recycleEntityId generator 0 = generator
recycleEntityId (EntityIdGenerator map i) entityId
	| i - 1 == entityId = cleanup (EntityIdGenerator map (i - 1))
	| otherwise = (EntityIdGenerator (Map.insert entityId () map) i)
	where
		cleanup :: EntityIdGenerator -> EntityIdGenerator
		cleanup generator@(EntityIdGenerator map i) =
			maybeCleanup generator $ Map.updateLookupWithKey (\_ _ -> Nothing) (i - 1) map
		
		maybeCleanup :: EntityIdGenerator -> (Maybe (), Map Word32 ()) -> EntityIdGenerator
		maybeCleanup (EntityIdGenerator _ i) ((Just _), map) = cleanup $ EntityIdGenerator map (i - 1)
		maybeCleanup generator (Nothing, _) = generator


newEntityIdGenerator :: EntityIdGenerator
newEntityIdGenerator = (EntityIdGenerator Map.empty 1)

availableEntityIds :: EntityIdGenerator -> Word32
availableEntityIds (EntityIdGenerator map i) = maxBound - i + (fromIntegral $ Map.size map)