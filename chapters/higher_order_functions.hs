-- higher-order functions
-- map, filter, foldl, foldr

import Data.Char

removes test [] = []
removes test (x:xs) = if test x
                      then removes test xs
                      else x:removes test xs

myProduct someList = foldl (*) 1 someList


sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x

myReverse xs = foldl rcons [] xs

myFoldl f init [] = init 
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x


-- we call it right fold because there are 2 arguments in a binary function:
-- a left argument and a right argument. 
-- the left fold compacts the list into the left argument
-- the right fold compacts the list into the right argument
-- => BEWARE OF USING foldr WHEN THE BINARY FUNCTION IS NOT COMMUTATIVE!!!
-- foldl is "more intuitive".
myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
    where rightResult = myFoldr f init xs



-- recreate the `elem` function using `filter` and `length`
myElem theList element = if filteredListLength == 1
                         then True
                         else False
    where filteredListLength = length (filter (\x -> x == element) theList)


-- reimplement the `isPalindrome` function from lesson 6. it should handle
-- sentences with spaces and/or capitals.
-- use `map` and `filter` 
-- isPalindrome :: [Char] -> [Char]
isPalindrome sentence = sanitizedSentence == reverse sanitizedSentence
    where sanitizedSentence = filter (\x -> x /= ' ') (map (toLower) sentence)



-- calculate the sum of a harmonic series of lenth n.
-- a harmonic series looks like this: 1/1 + 1/2 + 1/3 + 1/4 + ...
-- make sure to use lazy evaluation
harmonic n = foldl (+)  0 harmonicSeries
    where  harmonicSeries = map (\x-> 1/x) (take n [1..]) 


