module Main where

import ExifGPS.Get
import ExifGPS.Types

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Maybe

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



