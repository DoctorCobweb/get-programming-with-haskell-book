-- things learned
-- 1. work with file handles in haskell
-- 2. read and write files
-- 3. understand limitations of lazy evaluation for I/O

import System.IO

type FilePath = String

main :: IO ()
main = do
    helloFile <- openFile "hello.txt" ReadMode
    haveEOF1 <- hIsEOF helloFile
    firstLine <- if not haveEOF1
                 then hGetLine helloFile
                 else return "empty"
    putStrLn firstLine

    haveEOF2 <- hIsEOF helloFile 
    secondLine <- if not haveEOF2
                  then hGetLine helloFile
                  else return "empty"
    goodbyeFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyeFile secondLine
    hClose helloFile
    hClose goodbyeFile
    print "see ya"


