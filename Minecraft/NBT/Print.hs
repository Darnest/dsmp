module Minecraft.NBT.Print
	( showNBT
	, putShowNBT
	, hPutShowNBT
	) where
import Minecraft.NBT.Structure
import qualified Data.ByteString.UTF8 as BSUTF8
import System.IO

showNBT :: NBT -> String
showNBT nbt = (showNamedTag nbt 0)

putShowNBT :: NBT -> IO ()
putShowNBT = putStr . showNBT

hPutShowNBT :: Handle -> NBT -> IO ()
hPutShowNBT  h = (hPutStr h) . showNBT

showIndentation :: Int -> String
showIndentation level = concat $ replicate level "   "

showNamedTag :: NamedTag -> Int -> String
showNamedTag namedTag@(NamedTagEnd tag) indentation
	= showTag tag indentation
showNamedTag namedTag@(NamedTag nameLength name tag) indentation
	=  (showIndentation indentation)
	++ (tagName tag)
	++ "(\""
	++ (BSUTF8.toString name)
	++ "\"): "
	++ (showTagContents tag indentation)
	++ "\n"

showTagContents :: Tag -> Int -> String
showTagContents Tag_End indentation
	=  "Out of place, not explicitly prohibited by protocol"
showTagContents (Tag_Byte i) indentation
	= show i
showTagContents (Tag_Short i) indentation
	= show i
showTagContents (Tag_Int i) indentation
	= show i
showTagContents (Tag_Long i) indentation
	= show i
showTagContents (Tag_Float f) indentation
	= show f
showTagContents (Tag_Double f) indentation
	= show f
showTagContents (Tag_ByteArray length bs) indentation
	= "[" ++ (show length) ++ " bytes]"
showTagContents (Tag_String length bs) indentation
	= (BSUTF8.toString bs)
showTagContents (Tag_List tagType length tags) indentation
	=  (show length)
	++ " entries of type "
	++ (tagTypeName tagType)
	++ "\n"
	++ (showIndentation indentation)
	++ "{\n"
	++ (concat $ map (\t -> showTag t (indentation + 1)) tags)
	++ (showIndentation indentation)
	++ "}"
showTagContents (Tag_Compound tags) indentation
	=  (show (length tags))
	++ " entries\n"
	++ (showIndentation indentation)
	++ "{\n"
	++ (concat $ map (\t -> showNamedTag t (indentation + 1)) tags)
	++ (showIndentation indentation)
	++ "}"
	

showTag :: Tag -> Int -> String
showTag tag indentation
	=  (showIndentation indentation)
	++ (tagName tag)
	++ ": "
	++ (showTagContents tag indentation)
	++ "\n"

tagName :: Tag -> String
tagName = tagTypeName . tagType

tagTypeName :: Tag_Type -> String
tagTypeName Tag_Type_Byte = "TAG_Byte"
tagTypeName Tag_Type_End = "TAG_End"
tagTypeName Tag_Type_Short = "TAG_Short"
tagTypeName Tag_Type_Int = "TAG_Int"
tagTypeName Tag_Type_Long = "TAG_Long"
tagTypeName Tag_Type_Float = "TAG_Float"
tagTypeName Tag_Type_Double = "TAG_Double"
tagTypeName Tag_Type_ByteArray = "TAG_ByteArray"
tagTypeName Tag_Type_String = "TAG_String"
tagTypeName Tag_Type_List = "TAG_List"
tagTypeName Tag_Type_Compound = "TAG_Compound"