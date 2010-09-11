module Minecraft.NBT.Structure
	( NBT
	, Tag (..)
	, NamedTag (..)
	, Tag_Type (..)
	, tagType
	, tagTypeToId
	, tagTypeFromId
	) where
import qualified Data.ByteString as BS
import Data.Int

type NBT = NamedTag

data NamedTag
	= NamedTag Int16 BS.ByteString Tag
	| NamedTagEnd Tag
	deriving (Eq, Show)

data Tag
	= Tag_End
	| Tag_Byte Int8
	| Tag_Short Int16
	| Tag_Int Int32
	| Tag_Long Int64
	| Tag_Float Float
	| Tag_Double Double
	| Tag_ByteArray Int32 BS.ByteString
	| Tag_String Int16 BS.ByteString
	| Tag_List Tag_Type Int32 [Tag]
	| Tag_Compound [NamedTag]
	deriving (Eq, Show)

tagType :: Tag -> Tag_Type
tagType Tag_End = Tag_Type_End
tagType (Tag_Byte i) = Tag_Type_Byte
tagType (Tag_Short i) = Tag_Type_Short
tagType (Tag_Int i) = Tag_Type_Int
tagType (Tag_Long i) = Tag_Type_Long
tagType (Tag_Float f) = Tag_Type_Float
tagType (Tag_Double f) = Tag_Type_Double
tagType (Tag_ByteArray l bs) = Tag_Type_ByteArray
tagType (Tag_String l bs) = Tag_Type_String
tagType (Tag_List t l ts) = Tag_Type_List
tagType (Tag_Compound ts) = Tag_Type_Compound

data Tag_Type
	= Tag_Type_End
	| Tag_Type_Byte
	| Tag_Type_Short
	| Tag_Type_Int
	| Tag_Type_Long
	| Tag_Type_Float
	| Tag_Type_Double
	| Tag_Type_ByteArray
	| Tag_Type_String
	| Tag_Type_List
	| Tag_Type_Compound
	deriving (Enum, Eq, Show)

tagTypeToId :: Integral a => Tag_Type -> a
tagTypeToId Tag_Type_End = 0
tagTypeToId Tag_Type_Byte = 1
tagTypeToId Tag_Type_Short = 2
tagTypeToId Tag_Type_Int = 3
tagTypeToId Tag_Type_Long = 4
tagTypeToId Tag_Type_Float = 5
tagTypeToId Tag_Type_Double = 6
tagTypeToId Tag_Type_ByteArray = 7
tagTypeToId Tag_Type_String = 8
tagTypeToId Tag_Type_List = 9
tagTypeToId Tag_Type_Compound = 10

tagTypeFromId :: Integral a => a -> Maybe Tag_Type
tagTypeFromId 0 = Just Tag_Type_End
tagTypeFromId 1 = Just Tag_Type_Byte
tagTypeFromId 2 = Just Tag_Type_Short
tagTypeFromId 3 = Just Tag_Type_Int
tagTypeFromId 4 = Just Tag_Type_Long
tagTypeFromId 5 = Just Tag_Type_Float
tagTypeFromId 6 = Just Tag_Type_Double
tagTypeFromId 7 = Just Tag_Type_ByteArray
tagTypeFromId 8 = Just Tag_Type_String
tagTypeFromId 9 = Just Tag_Type_List
tagTypeFromId 10 = Just Tag_Type_Compound
tagTypeFromId i = Nothing