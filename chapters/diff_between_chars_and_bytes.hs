{-# LANGUAGE OverloadedStrings #-}

-- Summary Section 
-- Q 25.1
import System.Environment
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Random
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    bytes <- B.readFile fileName
    let bytesLength = B.length bytes
    let charsLength = T.length (E.decodeUtf8 bytes)
    print (bytesLength - charsLength)
