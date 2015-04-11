module ExifGPS.Types where

import Data.Word
import Data.Int
import qualified Data.ByteString as BS

-- The word is the marker code, and the string is it's name
type MarkerCode = (Word16,String)

-- Start of Image
soi :: MarkerCode
soi = (0xFFD8,"SOI")

-- End of Image
eoi :: MarkerCode
eoi = (0xFFD9,"EOI")

-- Padding
padding :: MarkerCode
padding = (0x0000,"Padding") 

-- Big Endian
big_endian :: MarkerCode
big_endian = (0x4D4D,"Big Endian")

-- Little Endian
little_endian :: MarkerCode
little_endian = (0x4949,"Little Endian")

-- APP, 0 -> F
app :: Int -> MarkerCode
app n = (0xFFE0 + fromIntegral n,"APP" ++ show n)

data ByteOrder = LittleEndian | BigEndian deriving Show


data Type = BYTE | ASCII | SHORT | LONG | RATIONAL 
          | UNDEFINED | SLONG | SRATIONAL deriving Show

num_bytes :: Type -> Word32
num_bytes SHORT = 2
num_bytes LONG = 4
num_bytes RATIONAL = 8
num_bytes SLONG = 4
num_bytes SRATIONAL = 8
num_bytes _ = 1

data IFD = IFD {
  byte_order :: ByteOrder,
  tag :: Word16,
  typ :: Type,
  cnt :: Word32,
  ost :: Word32,
  contains_data :: Bool
 } 

instance Show IFD where
 show ifd = tag_name (tag ifd) ++ " :: " 
             ++ show (typ ifd) ++ " (x"
              ++ show (cnt ifd) ++ ") at " 
               ++ show (ost ifd) ++ "\n"

tag_name :: Word16 -> String
tag_name 256 = "Image width"
tag_name 257 = "Image height"
tag_name 258 = "Number of bits per componenet"
tag_name 259 = "Compression scheme"
tag_name 262 = "Pixel compostion"
tag_name 274 = "Orientation of image"
tag_name 277 = "Number of components"
tag_name 284 = "Image data arrangement"
tag_name 530 = "Subsampling ratio of Y to C"
tag_name 531 = "Y and C positioning"
tag_name 282 = "Image resolution in width direction"
tag_name 283 = "Image resolution in height direction"
tag_name 296 = "Unit of X and Y resolution"
tag_name 273 = "Image data location"
tag_name 278 = "Number of rows per strip"
tag_name 279 = "Bytes per compressed strip"
tag_name 513 = "Offset to JPEG SOI"
tag_name 514 = "Bytes of JPEG data"
tag_name 301 = "Transfer function"
tag_name 318 = "White point chromaticity"
tag_name 319 = "Chromaticities of primaries"
tag_name 529 = "Color space transformation matrix coefficients"
tag_name 532 = "Pair of black and white reference values"
tag_name 306 = "File change date and time"
tag_name 270 = "Image title"
tag_name 271 = "Image input equipment manufacturer"
tag_name 272 = "Image input equipment model"
tag_name 305 = "Software used"
tag_name 315 = "Person who created the image"
tag_name 33432 = "Copyright holder"
tag_name 34665 = "Exif tag"
tag_name 34853 = "GPS tag"
tag_name x = case (attributeInfo x) of
 Just ai -> tagName ai
 Nothing -> "Unknown Tag " ++ show x

data Data = Byte Word16 [Word8]
          | Ascii Word16 String
          | Short Word16 [Word16]
          | Long Word16 [Word32]
          | Rational Word16 [(Word32,Word32)]
          | Undefined Word16 BS.ByteString
          | Slong Word16 [Int32]
          | Srational Word16 [(Int32,Int32)] 

instance Show Data where
 show (Byte tag bs) = show bs
 show (Ascii tag s) = s
 show (Short tag ss) = show ss
 show (Long tag ls) = show ls
 show (Rational tag rs) = show $ map (\(n,d) -> fromIntegral n / fromIntegral d) rs
 show (Slong tag ss) = show ss
 show (Srational tag ss) = show $ map (\(n,d) -> fromIntegral n / fromIntegral d) ss
 show (Undefined tag bs) = show (take n $ show bs) ++ "... (" ++ show l ++ " bytes in total)"
  where
  l = BS.length bs
  l' = length $ show bs
  n = if l' < 7 then l' else 7

emptyData :: Type -> Word16 -> Data
emptyData BYTE tag = Byte tag []
emptyData ASCII tag = Ascii tag ""
emptyData SHORT tag = Short tag []
emptyData LONG tag = Long tag []
emptyData RATIONAL tag = Rational tag []
emptyData UNDEFINED tag = Undefined tag BS.empty
emptyData SLONG tag = Slong tag []
emptyData SRATIONAL tag = Srational tag []

data Count = Count Int | Any

applyAny :: (Int -> Int -> Int) -> Count -> Count -> Count
applyAny _ Any _ = Any
applyAny _ _ Any = Any
applyAny f (Count x) (Count y) = Count (f x y)

instance Num Count where
 fromInteger = Count . fromInteger
 (+) = applyAny (+)
 (*) = applyAny (*)
 abs Any = Any
 abs (Count x) = Count (abs x)
 signum Any = Any
 signum (Count x) = Count (signum x)


data AttributeInformation = AI {
 tagName :: String,
 fieldName :: String,
 tagId :: Word16,
 fieldType :: Type,
 count :: Count
}

attributeInfo :: Word16 -> Maybe AttributeInformation
attributeInfo 0 = Just $ AI "GPS tag version" "GPSVersionId" 0 BYTE 4
attributeInfo 1 = Just $ AI "North or South Latitude" "GPSLatitudeRef" 1 ASCII 2
attributeInfo 2 = Just $ AI "Latitude" "GPSLatitude" 2 RATIONAL 3
attributeInfo 3 = Just $ AI "East or West Longitude" "GPSLongitudeRef" 3 ASCII 2
attributeInfo 4 = Just $ AI "Longitude" "GPSLongitude" 4 RATIONAL 3
attributeInfo 5 = Just $ AI "Altitude reference" "GPSAltitudeRef" 5 BYTE 1
attributeInfo 6 = Just $ AI "Altitude" "GPSAltitude" 6 RATIONAL 1
attributeInfo _ = Nothing