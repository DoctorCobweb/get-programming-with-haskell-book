-- things learned
-- 1. use do-notation to generate lists
-- 2. filter results in do-notation by using `guard`
-- 3. simplify do-notation with list comprehensions
--
-- what make using List as a Monad so interesting is that then you assign your list to a
-- variable using <-, you get to treat it as though it were a *single* value. the rest
-- of this code looks like it's operating on only one candidate, and yet the final result is 
-- the same as applying your logic to every candidate that is in the list.
--
-- the List as a Monad allows you to easily build complicated lists via list comprehensions.
--
-- the main use of the list monad it to quickly generate lists.


import Control.Monad
import Data.Char


powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return (2^value)


powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo = 2^value
    let powersOfThree = 3^value
    return (powersOfTwo, powersOfThree)


allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evenValue <- [2,4 .. n]
    oddValue <- [1,3 .. n]
    return (evenValue,oddValue)


numAndSquares :: Int -> [(Int,Int)]
numAndSquares n = do
    value <- [1 .. n]
    let squares = value^2
    return (value,squares)


-- using guard function from Control.Monad
-- which helps you filter values in a list
evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard(even value)
    return value


-- a version of standard filter function, except
-- using guards and leveraging list as a monad
filterGuard :: (a-> Bool) -> [a] -> [a]
filterGuard f vals = do
    val <- vals
    guard(f val)
    return val


----------------------------------------
-- 32.2 list comprehensions
----------------------------------------

evenSquares :: [Int]
evenSquares = do
    n <- [0 .. 9]
    let nSquared = n^2
    guard (even nSquared)
    return nSquared


-- list comprehensions are just specialized applications of monads (!)
-- haskell provides the list comprehension syntax as a more terse way
-- to generate lists, than to use the usual do-notation.
--
powersOfTwo' :: Int -> [Int]
powersOfTwo' n = [val^2 | val <- [1 .. n]]


powersOfTwoAndThree' :: Int -> [(Int,Int)]
powersOfTwoAndThree' n = [(powersOfTwo, powersOfThree) | val <- [1 .. n]
                                                       , let powersOfTwo = 2^val
                                                       , let powersOfThree = 3^val]


allEvenOdds' :: Int -> [(Int,Int)]
allEvenOdds' n = [(evenVal, oddVal) | evenVal <- [2,4 .. n]
                                    , oddVal <- [1,3 .. n]]


-- the guard function is completely abstracted away in list comprehensions
evensGuard' :: Int -> [Int]
evensGuard' n = [val | val <- [1 .. n], even val]


capitalizedWords = [newWord | word <- ["brown", "blue", "pink", "orange"]
                            , let capital = toUpper (head word)
                            , let newWord = [capital] ++ tail word]


----------------------------------------
-- summary questions
----------------------------------------
-- Q 32.1 generate a list of correct calendar dates using list comprehensions
--        want something like this:
--        calendar = [1,2,3....31,1,2,3,.....28,........, 1,2,3,...31]
--
--        given we know the num of days in each month
-- this is almost there... it has each month's dates inside another list.
-- aka calendar is a list of lists.
calendar = [[1 .. endDay] | month <- [0 .. 11]
                          , let endDay = [31,28,31,30,31,30,31,31,30,31,30,31] !! month]


monthEnds :: [Int]
monthEnds = [31,28,31,30,31,30,31,31,30,31,30,31]

datesDo :: [Int] -> [Int]
datesDo ends = do
    end <- ends
    dates <- [1 .. end]
    return dates


datesMonad :: [Int] -> [Int]
datesMonad ends = ends >>= (\end ->
                             [1 .. end] >>= 
                             (\date -> return date
                             )
                           )
                  




