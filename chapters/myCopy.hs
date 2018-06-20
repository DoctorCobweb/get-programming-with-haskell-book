{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Q 24.1
-- copy contents of first file into second file

main :: IO ()
main = do
    args <- getArgs
    let fileName1 = head args
    let fileName2 = args !! 1
    input1 <- TIO.readFile fileName1
    TIO.writeFile fileName2 input1
    print (mconcat ["copied ", fileName1, " to ", fileName2])
    


