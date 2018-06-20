-- things learned in this lesson
-- 1. access command line arguments
-- 2. use the traditional approach to interacting with I/O
-- 3. write I/O code using lazy evaluation to make I/O easier
--
-- think of streaming I/O as like lazy evaluation
--
-- 22.1 interacting with the command line the NON-lazy way

import System.Environment
import Control.Monad


-- the print function is (putStnLn . show)
-- and makes printing any type of value easier
--
--
main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
                      
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)



-- main :: IO ()
-- main = do
--     -- args <- getArgs
    
--     threeLines <- mapM (\x -> getLine) ["","",""]
--     mapM_ putStrLn threeLines

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM repeats action = mapM (\x -> action) [1 .. repeats]

-- 22.2 interacting with lazy I/O
--
