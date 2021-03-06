module Main where

import ExifGPS.Get
import ExifGPS.Types

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Word
import Data.Int

import Data.Maybe
import Numeric


files :: [FilePath]
files = ["test1.jpg","test2.jpg"]

testBs :: IO B.ByteString
testBs = B.readFile "test1.jpg"

test :: IO ()
test = mapM_ show_exif_data files

test2 :: IO [ExifData]
test2 = do
 bs <-  testBs
 let ds = runGet getExifData bs
 return ds

main = test

show_exif_data :: FilePath -> IO ()
show_exif_data file = do
 bs <- B.readFile file
 let app1 = runGet read_from_jpeg bs
 let (bo,ifds) = runGet (read_from_app1) app1
 let res = map (\ifd -> runGet (getIfdInfo ifd) app1) ifds
 let ss = map fst res
 putStrLn "TIFF data:"
 mapM_ (\s -> putStrLn $ "  " ++ s) ss
 let ofs = filter isJust $ map snd res
 let ifdss = map (\(Just (s,o)) -> (s,runGet (read_from_app1' bo o) app1)) ofs
 mapM_ (\(s,o) -> do
   let ress = map (\ifd -> runGet (getIfdInfo ifd) app1) o
   let sss = map fst ress
   putStrLn s
   mapM_ (\s -> putStrLn $ "  " ++ s) sss   
  ) ifdss

{-
get_gps_data_location :: B.ByteString -> [[IFD]]
get_gps_data_location jpegBs = gps_ifds
 where
  app1Bs = runGet read_from_jpeg jpegBs
  (byteOrder,ifds) = runGet read_from_app1 app1Bs
  res = map (\ifd -> runGet (getIfdInfo ifd) app1Bs) ifds
  ofs = catMaybes $ map snd res
  gpss = filter (\(a,_) -> a == gpsTag) ofs
  gps_ifds = map (\gps -> runGet (read_from_app1' byteOrder gps) app1Bs) (map snd gpss) 
-}
