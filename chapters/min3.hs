


-- using a multiargument function in IO using <$> and <*>
--
-- IO is also a member of the Applicative typeclass.


minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree v1 v2 v3 = min v1 (min v2 v3)


readInt :: IO Int
readInt = read <$> getLine


minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt



-- creating a user in the context of a maybe
-- 
-- you can create a User 2 ways
-- 1. via the Record syntax: User {name="dre", gamerId=12, score=123123}
-- 2. like a normal data constructor: User "dre" 12 123123
--
-- so you can partially apply arguments to the User data constructor and
-- thus Applicative and Functor are available to use.
data User = User { name :: String
                 , gamerId :: Int
                 , score :: Int
                 } deriving Show

-- but imagine if due to say server requests we sometimes get 
-- nothing back. then the User fields are wrapped in the Maybe context
serverUsername :: Maybe String
serverUsername = Just "dre"

serverGamerId :: Maybe Int
serverGamerId = Just 1333

serverScore :: Maybe Int
serverScore = Just 9001

-- this is then how you handle creating a user that has its fields in 
-- the Maybe context and also wrapped in the same Maybe context
theUser = User <$> serverUsername <*> serverGamerId <*> serverScore -- Just (User {name="dre,.....})



main :: IO ()
main = do
    putStrLn "enter 3 numbers, each followed by enter."
    smallest <- minOfInts
    print (show smallest)



