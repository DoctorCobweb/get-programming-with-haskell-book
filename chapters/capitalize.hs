{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Q 24.2
-- capitalize the text in a file. write back to same file.

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    input <- TIO.readFile fileName
    let capitalized = T.toUpper input
    TIO.writeFile fileName capitalized
    print (mconcat ["capitalized the file: ", fileName])
