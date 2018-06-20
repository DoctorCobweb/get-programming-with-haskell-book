-- interacting with lazy I/O



--part of `split` library. installed via stack
import Data.List.Split 


mainTwo :: IO ()
mainTwo = do
    userInput <- getContents
    let numbers = toInts userInput
    print ( sumOfSquares numbers) 

-- this function doesn't return the answer to user. 
-- i dont know why
reverser :: IO ()
reverser = do
    userInput <- getContents
    -- let reverseUserInput = reverse userInput
    putStrLn (reverse userInput) 


----------------------------------------
-- 22.2.1 thinking of your problem as a lazy list
----------------------------------------

toInts :: String -> [Int]
toInts val = map read  (lines val)

sumOfSquares :: [Int] -> Int
sumOfSquares vals = sum (map (^2) vals)


main :: IO ()
main = do
    userInput <- getContents
    let operation = head userInput
    let numbers = toInts (tail userInput)
    let result = if operation == '+'
                 then foldl (+) 0 numbers
                 else if operation == '*'
                      then foldl (*) 0 numbers
                      else 0 

    print result
    







