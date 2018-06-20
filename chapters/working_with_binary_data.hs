{-# LANGUAGE OverloadedStrings #-}

-- will learn
-- 1. use the `ByteString` type to efficiently deal with binary data
-- 2. treat `ByteString`s as regular ASCII strings by using
--    Bytestring.Char8
-- 3. use haskell to glitch JPEGs
-- 4. work with binary Unicode data
--
-- `ByteString` allows you to treat raw binary data as though
-- it were a regular string. it's an efficient way to deal
-- with any streams of binary data.
--
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Random
import Control.Monad

-- sampleBytes :: B.ByteString
-- sampleBytes = "Hello!"

-- sampleString :: String
-- sampleString = BC.unpack sampleBytes

bcInt :: BC.ByteString
bcInt = "6"

toInts :: BC.ByteString -> Int
toInts bs = read (BC.unpack bs)

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255

-- BC.pack expects a string => list of Char
intToBC :: Int -> BC.ByteString
intToBC c = BC.pack [intToChar c]

-- quick check quiz 25.3
randomChar :: IO Char
randomChar = do
    randomInt <- randomRIO (0,255)
    return (toEnum randomInt)

-- a deterministic version of your random goal.
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
    where (before,rest) = BC.splitAt loc bytes
          newChar = intToBC charVal 
          after = BC.drop 1 rest

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1,bytesLength)
    randomByte <- randomRIO (0,255)
    return (replaceByte location randomByte bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before,changed,after]
    where (before,rest) = BC.splitAt start bytes
          (target,after) = BC.splitAt size rest
          changed = (BC.sort target)
          -- changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 10 
    let bytesLength = BC.length bytes
    randomStartLocation <- randomRIO (0, bytesLength - sectionSize)
    return (sortSection randomStartLocation sectionSize bytes)

glitchActions :: [BC.ByteString -> IO BC.ByteString]  
glitchActions = [ randomReplaceByte
                , randomSortSection
                , randomReplaceByte
                , randomSortSection
                , randomReplaceByte
                ]

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
    let glitchedFileName = mconcat ["glitched_",fileName]
    BC.writeFile glitchedFileName glitched
    print "all done"


blahText :: T.Text
blahText = "blah"

blahSafe :: B.ByteString
blahSafe = E.encodeUtf8 blahText


-- to be safe, never use the convenience of Data.ByteString.Char8 if 
-- you're working with data that may contain Unicode.
-- stick to ByteString, Text, and Text.Encoding for this case.
--
-- when youre working with purely binary data, then the
-- combination of regular ByteStrings and Char8 works great.
