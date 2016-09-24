module Main where

import ExifGPS.Get
import ExifGPS.Types

-- | An example usage of the ExifGPS modules to extract the Exif data from the two sample images in the repository
main :: IO ()
main = do
 s1 <- test1
 putStrLn $ show s1
 s2 <- test2
 putStrLn $ show s2

test1 :: IO ShowExif
test1 = readExifData "test1.jpg"

test2 :: IO ShowExif
test2 = readExifData "test2.jpg"



