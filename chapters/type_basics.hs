-- this lesson introduces one of the most powerful aspects of haskell: its
-- robust type system.
--
-- Int type IS BOUNDED by memory limiations
x :: Int
x = 2

-- Integer type is NOT bounded
y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99, 0.7,0.4]

letters :: [Char]
letters = ['a','b','c']

aPet :: [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34,45)

firstLastMiddle :: (String,String,Char)
firstLastMiddle = ("oscar","grouch",'d')

streetAddress :: (Int,String)
streetAddress = (123, "blah st")

doubleIt :: Int -> Int
doubleIt n = n *2

-- you don't have to cast the `2` value because literal numbers
-- in haskell are polymorphic: their type is determined from the compiler based
-- on the way they're used.
half :: Int -> Double
half n = (fromIntegral n) / 2

halve :: Integer -> Integer
halve n = n `div` 2


printDouble :: Int -> String
printDouble b = show (doubleIt b)

-- multiple arguments for a function
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number,street, town)

-- type variables
simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)

-- just as with regular variables, using different names for type variables DOESNT imply that the values
-- represented by the variables must be different, only that they can be.
f1 :: a -> a -- has to be like Int -> Int, Char -> Char etc
f2 :: a -> b -- can be Int -> Int, Char -> Int, Int -> Bool, Char -> Bool, Bool -> Bool










