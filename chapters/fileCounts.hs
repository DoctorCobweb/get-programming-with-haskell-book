import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO



-- these are useful standard functions for handling files. they hide away
-- many of the details of reading, writing, and appending files.
-- CAVEAT: readFile does not close the file handle! (havnt looked into 
-- whether writeFile and appendFile behave similarly)
-- readFile :: FilePath -> IO String
-- writeFile :: FilePath -> String -> IO ()
-- appendFile :: FilePath -> String -> IO ()


getCounts :: String -> (Int,Int,Int)
getCounts fileContents = (charCount, wordCount, lineCount)
    where charCount = length fileContents
          wordCount = (length . words) fileContents
          lineCount = (length . lines) fileContents

countsText :: (Int,Int,Int) -> String
countsText (cCount, wCount, lCount) = yadda
    where yadda = unwords ["chars: "
                          , show cCount
                          , " words: "
                          , show wCount
                          , " lines: "
                          , show lCount
                          ]

-- main :: IO ()
-- main = do
--     args <- getArgs
--     let fileName = head args
--     input <- readFile fileName
--     let summary = (countsText . getCounts) input
--     appendFile "stats.dat" (mconcat [fileName, ": ", summary, "\n"])
--     print summary

-- lazy with fix for placing the close file handler in 
-- the correct spot
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let fileName = head args
--     file <- openFile fileName ReadMode
--     input <- hGetContents file
--     let summary = (countsText . getCounts) input
--     putStrLn summary
--     hClose file 
--     appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])


-- strict I/O is the best fix for the problems above. use this in conjunction
-- with Data.Text, rather than with String.
-- Data.Text is, remember, a strict data type (ie wont use lazy evaluation).
-- here's how to rewrite the above code using strict I/O:

getCountsStrict :: T.Text -> (Int,Int,Int)
getCountsStrict input = (cCount, wCount, lCount)
    where cCount = T.length input
          wCount = (length . T.words) input
          lCount = (length . T.lines) input

countsTextStrict :: (Int,Int,Int) -> T.Text
countsTextStrict (cc,wc,lc) = T.pack (unwords ["chars: "
                                        , show cc
                                        , " words: "
                                        , show wc
                                        , " lines: "
                                        , show lc])

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    file <- TIO.readFile fileName
    let summary = (countsTextStrict . getCountsStrict) file
    -- TIO.hClose file
    TIO.appendFile "stats.dat" (mconcat [(T.pack fileName), T.pack " ", summary, T.pack "\n"])
    TIO.putStrLn summary

