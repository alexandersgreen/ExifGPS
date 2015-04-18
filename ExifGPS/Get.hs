module ExifGPS.Get where

import ExifGPS.Types

import Control.Monad
import Data.Binary.Get
import Data.Word
import Data.Char
import Data.Bits

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS

getWord16bo :: ByteOrder -> Get Word16
getWord16bo LittleEndian = getWord16le
getWord16bo BigEndian = getWord16be

getWord32bo :: ByteOrder -> Get Word32
getWord32bo LittleEndian = getWord32le
getWord32bo BigEndian = getWord32be

getByteOrder :: Get ByteOrder
getByteOrder = do
 w <- getWord16be
 case w of
  0x4949 -> return LittleEndian
  0x4D4D -> return BigEndian
  w -> do
   bs <- getByteString 8
   error $ "getByteOrder: " ++ show w ++ "\n" ++ show bs

-- Check that the give jpeg marker is the next word
check_marker_bo :: ByteOrder -> MarkerCode -> Get ()
check_marker_bo bo (word,name) = do
 word' <- getWord16bo bo
 case word == word' of
  True -> return ()
  False -> error $ "check_marker: " ++ name ++ ", " ++ show word' ++ " should be " ++ show word 

check_marker :: MarkerCode -> Get ()
check_marker = check_marker_bo BigEndian

-- Check that the given ASCII string is the next data
check_string :: String -> Get ()
check_string s = do
 b <- check_string' s
 if b then return () else error $ "check_string: " ++ s
 where
  check_string' [] = return True
  check_string' (c:s) = do
   b <- getWord8
   let c' = chr $ fromIntegral b
   case c == c' of
    True -> check_string' s
    False -> return False

-- check the file starts with the SOI, 
getToApp1 :: Get Word16
getToApp1 = do
 check_marker soi
 check_marker $ app 1
 length <- getWord16be
 check_string "Exif"
 check_marker padding
 return length

getString :: Int -> Get String
getString n = do
  s <- getString' "" n 
  return (reverse s)
 where
  getString' :: String -> Int -> Get String
  getString' s 0 = return s
  getString' s n = do
   b <- getWord8
   let c' = chr $ fromIntegral b
   getString' (c':s) (n-1)
   
getType_bo :: ByteOrder -> Get Type
getType_bo bo = do
 typ <- getWord16bo bo
 let typ' = case typ of
             1 -> BYTE
             2 -> ASCII
             3 -> SHORT
             4 -> LONG
             5 -> RATIONAL
             7 -> UNDEFINED
             9 -> SLONG
             10 -> SRATIONAL
             x -> error $ "Unknown Type: " ++ show x
 return typ'
 
getIFDbo :: ByteOrder -> Get IFD
getIFDbo bo = do
 tag <- getWord16bo bo
 typ <- getType_bo bo
 cnt <- getWord32bo bo
 ost <- getWord32bo bo
 return $ IFD {byte_order = bo, tag = tag, typ = typ, cnt = fromIntegral cnt, ost = fromIntegral ost, contains_data = cnt * num_bytes typ <= 4}

getDataOfType :: Word16 -> ByteOrder -> Int -> Type -> Get ExifData
getDataOfType tag _ n ASCII = do
 s <- getString n
 return $ Ascii tag s
getDataOfType tag bo n UNDEFINED = do
 bs <- getByteString n
 return $ Undefined tag bs
getDataOfType tag bo n t = do
  getData (emptyData t tag) n
 where
  getData :: ExifData -> Int -> Get ExifData
  getData ds 0 = return (reverseData ds)
  getData ds n = do
   ds' <- getOne ds 
   getData ds' (n-1)
  getOne :: ExifData -> Get ExifData
  getOne (Byte tag bs) = do
   b <- getWord8
   return (Byte tag (b:bs))
  getOne (Short tag ss) = do
   s <- getWord16bo bo
   return (Short tag (s:ss))
  getOne (Long tag ls) = do
   l <- getWord32bo bo
   return (Long tag (l:ls))
  getOne (Rational tag rs) = do
   n <- getWord32bo bo
   d <- getWord32bo bo
   return (Rational tag ((n,d):rs))
  getOne (Slong tag ss) = do
   s <- getWord32bo bo
   return (Slong tag ((fromIntegral s):ss))
  getOne (Srational tag ss) = do
   n <- getWord32bo bo
   d <- getWord32bo bo
   return (Srational tag (((fromIntegral n),(fromIntegral d)):ss))
  reverseData :: ExifData -> ExifData
  reverseData (Byte tag bs) = Byte tag $ reverse bs
  reverseData (Ascii _ _) = error "reverseData called on ASCII"
  reverseData (Short tag ss) = Short tag $ reverse ss
  reverseData (Long tag ls) = Long tag $ reverse ls
  reverseData (Rational tag rs) = Rational tag $ reverse rs
  reverseData (Undefined _ _) = error "reverseData called on UNDEFINED"
  reverseData (Slong tag ss) = Slong tag $ reverse ss
  reverseData (Srational tag ss) = Srational tag $ reverse ss

getIfdData :: IFD -> Get ExifData
getIfdData ifd = do
   case (contains_data ifd) of
            True -> do
                     let the_data = ost ifd
                     return $ case typ ifd of
                               BYTE -> Byte (tag ifd) $ take (fromIntegral (cnt ifd)) [fromIntegral $ the_data,fromIntegral $ shiftR the_data 4,fromIntegral $ shiftR the_data 8,fromIntegral $ shiftR the_data 12]
                               ASCII -> Ascii (tag ifd) $ take (fromIntegral (cnt ifd)) [chr $ fromIntegral $ the_data,chr $ fromIntegral $ shiftR the_data 4,chr $ fromIntegral $ shiftR the_data 8,chr $ fromIntegral $ shiftR the_data 12]
                               SHORT -> Short (tag ifd) $ take (fromIntegral (cnt ifd)) [fromIntegral $ the_data,fromIntegral $ shiftR the_data 8]
                               LONG -> Long (tag ifd) [the_data]
                               UNDEFINED -> Undefined (tag ifd) $ BS.pack $ take (fromIntegral (cnt ifd)) [fromIntegral $ the_data,fromIntegral $ shiftR the_data 4,fromIntegral $ shiftR the_data 8,fromIntegral $ shiftR the_data 12]
                               t -> error "The impossible happened"
            False -> do
                      skip $ fromIntegral $ ost ifd
                      getDataOfType (tag ifd) (byte_order ifd) (fromIntegral $ cnt ifd) (typ ifd)

getExifIFDs :: Get ([IFD],ByteOrder)
getExifIFDs = do
 bo <- getByteOrder
 check_marker_bo bo (42,"42") 
 offset <- getWord32bo bo
 let offset' = fromIntegral $ offset - 8
 skip offset'
 count <- getWord16bo bo
 ifds <- replicateM (fromIntegral count) (getIFDbo bo)
 return (ifds,bo)

getExifData :: Get [ExifData]
getExifData = do
 getToApp1
 (ifds,bo) <- lookAhead $ getExifIFDs
 datas <- mapM (lookAhead . getIfdData) ifds
 extras <- mapM (lookAhead . getExtraData bo) datas
 return (datas ++ concat extras)

extraDataTags :: [Word16]
extraDataTags = [34665,34853]

getExtraData :: ByteOrder -> ExifData -> Get [ExifData]
getExtraData bo (Long tag [l]) = 
 case (elem tag extraDataTags) of
  True -> do
   ifds <- lookAhead $ do
    skip (fromIntegral l)
    count <- getWord16bo bo
    replicateM (fromIntegral count) (getIFDbo bo)
   mapM (lookAhead . getIfdData) ifds
  False -> return []
getExtraData _ _ = return []

getData :: (FromExifData a, FromList b)  => Get (b a)
getData = do
 exifData <- getExifData
 return $ fromList $ map fromExifData exifData 

readExifData :: (FromExifData a, FromList b) => FilePath -> IO (b a)
readExifData file = do
 bs <- B.readFile file
 return $ runGet getData bs