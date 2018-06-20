



-- Q22.2



-- my solution, nowhere close.
-- quoteForNum :: Int -> String
-- quoteForNum 1 = "blah 1"
-- quoteForNum 2 = "blah 2"
-- quoteForNum 3 = "blah 3"
-- quoteForNum 4 = "blah 4"
-- quoteForNum 5 = "blah 5"
-- quoteForNum _ = "yadda"

-- toInts :: String -> [Int]
-- toInts = map read . lines

-- main :: IO ()
-- main = do
--     putStrLn "please select between 1 and 5 (endpoints included) for a quote. press n to stop"
--     userInput <- getContents
--     let val = toInts userInput
--     putStrLn (quoteForNum (head val))
--     

-- from back of book
quotes :: [String]
quotes = ["quote 1"
          ,"quote 2"
          ,"quote 3"
          ,"quote 4"
          ,"quote 5"]
          
lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : (lookupQuote xs)
   where quote = quotes !! (read x - 1)

main :: IO ()
main = do
    userInput <- getContents
    mapM_ putStrLn (lookupQuote  (lines userInput))
