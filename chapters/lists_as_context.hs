-- things learned
-- 1. explain the formal definition of the Applicative typeclass
-- 2. represent parameterized types as either containers or contexts
-- 3. use List as a context to explore nondeterministic computing
--
-- this is the typeclass definition for Applicative
--
-- class Functor f => Applicative f where
--     (<*>) :: f (a->b) -> f a -> f b
--     pure :: a -> f a
--
-- the pure function takes an ordinary value or function, and
-- wraps it in the Applicative context. you always have a way
-- to take an ordinary type and put it in a context.
--
-- pure 3 :: Maybe Int
--
-- => Just 3
--
-- you can also use pure to put a function into the context of Applicative.
-- so you can do this
--
-- pure (6+) <*> Just 5
-- => Just 11
--
-- which is equivalent to
-- (6+) <$> Just 5
-- => Just 11
--
-- Quick check 29.2
-- make the String "hello" into an  IO String
-- pure "hello" :: IO String
-- 
----------------------------------------
-- 29.2 containers vs context
----------------------------------------
-- need to be clearer about the distinction between 
-- parameterized types that represent contexts or containers.
--
-- we need to do this because Applicative and Monad (later) 
-- make sense only as context.
--
-- 1. parameterized types that represent a container are types that
--    represent a data structure
--
-- 2. when a type is in a context, extra information is implied about
--    the type, beyond its structure
--
--
-- List is commonly viewed as a container type, but it's also a context
-- type. if you understand how List can be both a container and a
-- context, then you're on the path to truly understanding Applicative.
-- remember the power of Applicative is that it lets you apply a function
-- in a parameterized type.
--
--
----------------------------------------
-- 29.3 List as a context
----------------------------------------
-- List is a member of Applicative, so there must be a way to view List
-- as a context.
--
-- the best way to understand List as a context is that it describes 
-- nondeterministic computation. in nondeterministic computing you're
-- computing multiple possibilities all at once.
--
-- this is how you'd think nondeterministically: when you add 2 values
-- in the context of a list, you're adding together ALL possible values
-- from the 2 contexts
--
-- pure (+) <*> [10,20,30] <*> [50,200]
-- => [50,210,70,220,80,230]
--
-- let's point out the major differences between a list as a container
-- and a list as a context:
--
-- 1. a list as a container is a sequence of values that can hold any type.
--    each item in the list points to the next one or to the empty list
-- 
-- 2. a list as a context represents a set of possibilities. think
--    of a list as a context as being a single variable that can 
--    contain many possible values


doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrize :: [Int]
boxPrize = [10,50]

-- prizeMultiplier :: [Int]
-- prizeMultiplier = [10,50]

totalPrizeMultiplier = pure (*) <*> doorPrize <*> boxPrize

primesToN :: Int -> [Int]
primesToN n = primes 
    where composites = pure (*) <*> [2 .. n] <*> [2 .. n]
          primes = filter (\x -> not (x `elem` composites)) [2 .. n] 

-- quickly create large amounts of test data
data User = User { name :: String
                 , gamerId :: Int
                 , score :: Int
                 } deriving Show

testNames :: [String]
testNames = [ "hoby smith"
            , "bob; DROP TABLE Students; blah"
            , "christina NULL"
            , "randall blah"
            , "yadda blah"
            , "henry ford"
            , "wickemm bladly"
            , "cyborg general"
            ]

testIds :: [Int]
testIds = [1337, 101, 44444, 123, 449, 330, 0]

testScores :: [Int]
testScores = [0, 10000, -99999, 707, 808, 909, 1001, 10023]

testData :: [User]
testData = (pure User) <*> testNames <*> testIds <*> testScores




----------------------------------------
-- Summary questions
----------------------------------------
--
-- Q29.1 
allMap :: Applicative f => (a -> b) -> f a -> f b
allMap = fmap

-- Q 29.2
example :: Int
example = (*) ((+) 2 4) 6

maybeExample :: Maybe Int
maybeExample = pure example

-- Q 29.3 not sure if correct..

add4 :: Int -> Int -> Int -> Int -> Int
add4 a b c d = a + b + c + d

boughtBeer :: [Int]
boughtBeer = [6,12]

drankAlready :: [Int]
drankAlready = [-4]

friendsOver :: [Int]
friendsOver = [2,3]

aveDrink :: [Int]
aveDrink = [3,4]

howManyBeers = pure add4 <*> boughtBeer <*> drankAlready <*> friendsOver <*> aveDrink























