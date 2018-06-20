-- things learned
-- 1. understand the limitations of both Functor and Applicative
-- 2. use Monads operator ( >>= ) to chain together functions in a context
-- 3. write IO code without the do-notation
--
-- Functor allows you to change individual values in a context
-- (+2) <$> Just 3
-- => Just 5
--
-- Applicative increase your power by enabling you to use partial application 
-- in a context. this allows you to use multiple arguments in a context
--
-- pure (+) <*> Just 3 <*> Just 2
-- => Just 5
--
-- the final step in the evolution of this process of increasing your
-- power for application in a context, we introduce the Monad typeclass.
-- the Monad will allow you to perform any arbitrary computation in a 
-- context you'd like.
--
-- the do-notation is an example of syntactic-sugar for the monad, which
-- from previous experience using it, has shown how powerful chaining 
-- as many computations as you'd like in a context, can really be.
--
-- to understand how incredible Monads are, we will suspend the use of
-- do-notation.
--
--
--
------------------------------------------
-- 30.1.1 combining two Map lookups
------------------------------------------
-- each user is identified with a unique GamerId :: Int
-- each GamerId associates with a unique Username :: String
-- the Username is used to lookup credits for the user.
--
import qualified Data.Map as Map

type WillCoId = Int
type GamerId = Int
type UserName = String
type PlayerCredits = Int


gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1)
                         ,(1002,2)
                         ,(1003,3)
                         ,(1004,4)
                         ,(1005,5)
                         ,(1006,6)]

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                         ,("KINGinYELLOW",15000)
                         ,("dagon1997",300)
                         ,("rcarter1919",12)
                         ,("xCTHULHUx",50000)
                         ,("yogSOThoth",150000)]


lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId willId = Map.lookup willId gamerIdDB

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits user = Map.lookup user creditsDB

-- missing function needed in order to perform creditsFromId
-- Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits
--
-- so like 
--
-- Applicative f => f a -> (a -> f b) -> f b
--
-- this type signature is exremely close to Monad's type signature... see below

altLookupCredit :: Maybe UserName -> Maybe PlayerCredits
altLookupCredit Nothing = Nothing
altLookupCredit (Just user) = lookupCredits user

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredit (lookupUserName id)

creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id

------------------------------------------
-- 30.2 the bind operator >>=
------------------------------------------
-- check out the type signature for the bind operator
--
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
--
-- this looks pretty much exactly like the one we were after in the above
-- section. the only difference is that the Applicative type constraint has
-- been replace with the Monad m type constraint
--
-- so >>= is a member of the Monad type class...
--
-- let's rewrite the creditsFromId function to use the bind operator
creditsFromIdBind :: GamerId -> Maybe PlayerCredits
creditsFromIdBind id = lookupUserName id >>= lookupCredits


creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId wid = lookupGamerId wid >>= lookupUserName >>= lookupCredits


-- creating an echo function with IO type
echo :: IO ()
echo = getLine >>= putStrLn


-- we now have the final piece of the puzzle, the bind operator.
-- with the following operators, we can chain together any computation
-- you need in any context
--
-- 1. <$>
-- 2. <*>
-- 3. pure
-- 4. >>=


askForName :: IO ()
askForName = putStrLn "give us your name"

getName :: IO String
getName = getLine

nameStatement :: String -> String
nameStatement name = "hello, " ++ name ++ "!"

putName :: String -> IO ()
putName name = putStrLn name



makeGreeting :: IO ()
makeGreeting = askForName >> 
               getName >>= 
               (\name -> 
                 return (nameStatement name)) >>= 
               putStrLn




------------------------------------------
-- Summary questions
------------------------------------------
-- Q 30.1
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f val = val >>= (\x -> return (f x))

-- Q 30.2
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp fWrapped val = fWrapped >>= ( \f -> val >>= (\x -> return (f x)) )


