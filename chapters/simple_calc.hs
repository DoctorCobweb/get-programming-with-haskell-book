
import Data.List.Split
--Q22.1

-- lazy I/O lets you treat your input like a list
-- sampleInput :: [String]
-- sampleInput = ["21","+","123"]
--
calc :: [String] -> Int
calc (val1:"+":val2:rest) = read val1 + read val2
calc (val1:"*":val2:rest) = read val1 * read val2
-- calc _ = 0 -- some bad default val, i know.

main :: IO ()
main = do
    userInput <- getContents
    let expressions = map (splitOn " ") . lines
    let values = expressions userInput
    mapM_ print (map calc values)

