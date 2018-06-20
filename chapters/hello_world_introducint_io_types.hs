-- whenever a function uses IO, the results of that function are FOREVER 
-- marked as coming from IO.
--
-- IO () represents an *action*, not a typical type.
-- what is meant by action is that IO actions work
-- much like functions but they don't always return
-- the same value for a fixed input value. this
-- means that IO actions disobey referential
-- transparency.

import System.Random


mystery :: Int -> Int -> IO Int
mystery v1 v2 = do
    putStrLn "enter a num"
    val3Input <- getLine
    let v3 = read val3Input
    return ((v1 + v2 + v3)^2)

-- things learned in this lesson
-- 1. understand how haskell handles I/O by using IO types
-- 2. us do-notation to perform I/O
-- 3. write pure programs that interact with the real world
--

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

-- main :: IO ()
-- main  = do
--     dieRoll <- randomRIO (minDie,maxDie)
--     putStrLn (show dieRoll)

-- because I/O is so dangerous and unpredictable, after you have a value 
-- come from I/O, haskell doesn't allow you to use that value outside of
-- the context of the IO type.
--
-- for example, if you fetch a random number using randomRIO, you can't
-- use that value outside `main` or a similar IO action.
--
-- so haskell separates I/O logic from pure code. pure code obeys haskell
-- idioms like referential transparency.
--
------------------------------------------
-- 21.2 do-notation
------------------------------------------
-- the do-notation allows you to succinctly write code
-- that is withing the context of IO.
--
-- 1. the '<-' operator allows you to treate an 'IO a'
--    type like it is an 'a' type
--   
--
--    eg. name <- getLine
--
--    getLine has type IO String
--    name has type String
--
-- 2. the 'let' keyword allows you to create variables
--    in the do-block which are NOT IO types.
--
--    eg. blahName = helloPerson name
--
--    and helloPerson :: String -> String, so blahName is type String.
--

