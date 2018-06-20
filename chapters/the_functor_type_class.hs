-- things in this lesson
-- 1. be able to use the `Functor` typeclass
-- 2. solve problems with `fmap` and <$>
-- 3. understand kinds for `Functors`
-- 
-- the Functor typeclass provides a generic interface for applying functions to
-- values in a container or context.
-- 
-- "Functor allows you to generalize by solving a single problem the
--  same way in multiply different parameterized types."
--
-- parameterized types represent a context eg. Maybe for missing values,
-- and IO for values that come from the I/O world. so Functors are all about
-- provide a clear way to use 'normal, and boring' functions like Int->String,
-- on types with context, thus on parameterized types.
--
-- here are some members of Functors
-- 1. List
-- 2. Map
-- 3. Maybe
-- 4. IO

import Data.Maybe
import qualified Data.Map as Map
import Data.Char

successfulRequest :: Maybe Int
successfulRequest = Just 8


failedRequest :: Maybe Int
failedRequest = Nothing


incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n+1)
incMaybe Nothing = Nothing

-- quickcheck 27.1
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just blah) = Just (reverse blah)
reverseMaybe Nothing = Nothing

reverseMaybe' :: Maybe String -> Maybe String
reverseMaybe' m = reverse <$> m

-- instead of providing custom functions like the above for every 
-- possible parameterized type, the haskell gods have created an 
-- extremely clever 'design pattern' called the Functor typeclass.
-- it goes like this:
-- 1. make your type, which has a context, an instance of the Functor
--    typeclass provide the implementation for one function: 'fmap'
--  
--    fmap :: Functor f => (a -> b) -> f a -> f b
--
--    Functor f  means f is a Functor type in need of a type parameter
--               ie. Functors must usually be of kind * -> *
--                   exception to this rule is Map. 
--                   Map has kind * -> * -> * but when it's made
--                   a Functor instance, we forget about the keys and
--                   just look at using its values during our fmap definition
--                  
--    f a   is a Functor of type a              eg Maybe Int
--    f b   is also a Functor, but of type b    eg Maybe Double
--
-- 2. then fmap allows you to use your function on a type with context. 


-- this is how Maybe is made to be a Functor instance
-- instance Functor Maybe where
--     fmap func (Just n) = Just (func n)
--     fmap func Nothing = Nothing

-- this is how to use it
-- fmap (+1) failedRequest
-- fmap (+1) successfulRequest
-- (+1) <$> successfulRequest   -- <$> binary operator version of fmap
-- (+1) <$> failedRequest

--23.3.1 one interface for four problems

data RobotPart = RobotPart { name :: String
                           , description :: String 
                           , cost :: Double
                           , count :: Int
                           } deriving Show

leftArm :: RobotPart
leftArm = RobotPart { name = "left arm"
                    , description = "left arm for face punching!"
                    , cost = 1000.00
                    , count = 3
                    }

rightArm :: RobotPart
rightArm = RobotPart { name = "right arm"
                     , description = "right arm for kind hand gestures"
                     , cost = 1025.00
                     , count = 5
                     }

robotHead :: RobotPart
robotHead = RobotPart { name = "robot head"
                      , description = "this head looks mad"
                      , cost = 5032.23
                      , count = 2
                      }

-- code for rendering a RobotPart as an HTML snippet
type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [ "<h2>",partName,"</h2>"
                          , "<p><h3>desc</h3>",partDesc
                          , "</p><p><h3>cost</h3>"
                          , partCost
                          , "</p><p><h3>count</h3>"
                          , partCount, "</p>"
                          ]
    where partName = name part
          partDesc = description part
          partCost = show (cost part)
          partCount = show (count part)


-- so we will have 4 scenarios of trying to convert a RoboPart to HTML.
-- these 4 cases will be different parameterized types.
-- the first case is using the Map type to create a partsDB, an internal
-- database of RobotParts.
-- Map is a good example becuase it involves 3 instances of Functor
-- 1. it's made from a List
-- 2. returns Maybe values
-- 3. Map is itself a Functor

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1,2,3]
          values = [leftArm, rightArm, robotHead]
          keyVals = zip keys values

-- next up is solving the problem of creating the following function,
-- which will insert some HTML to a page's template for display:
--
--      insertSnippet :: Maybe Html -> IO ()
--
-- here's an example of fetching a RobotPart from the partsDB
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- this is the juicy bit. 
-- because partVal is a Maybe type that means partVal is also an instance
-- of Functor. And renderHtml is a 'normal' function so you can use fmap or
-- equivalently <$>
--
-- the advantage here is that the Maybe context is *kept* after the fmap'ing.
-- partVal may or may not be there in the database, but we don't really have
-- to worry about it as the Maybe context keeps track of that matter.
-- we just code away; we can easily pass partHtml to insertSnippet.
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal


-- get a list of parts from the db. since List is a Functor,
-- fmap/<$> is actually the map function so we can write 
-- allParts using Functor specific language
allParts :: [RobotPart]
allParts =  snd <$> Map.toList partsDB
-- allParts = map snd (Map.toList partsDB) -- above line is equiv to this line

-- List is also a Functor instance. fmap/<$> is the usual map function
-- using fmap keeps the context, here it's the List context
allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts


-- we can make partsDB better: keep the HTML version of a part
-- in the db. so you'd have a htmlPartsDB.
-- turns out that Map is also a Functor instance, which means we 
-- can 'map' over it using fmap/<$> to convert partsDB to htmlPartsDB
--
-- by leveraging the fact that Map is a Functor instance we easily
-- make the conversion from Map.Map Int RobotParts to Map.Map Int Html
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB


-- 27.3.5 transforming an IO RobotPart to IO Html
-- a RobotPart may come from IO.
-- to simulate this case we use the 'return' function
-- to create an IO type of a RobotPart
-- hardcoding stuff in
leftArmIO :: IO RobotPart
leftArmIO = do
    return leftArm


-- now we want to turn leftArmIO to IO Html so
-- we can write the HTML snippet to a file.
-- well IO is also a Functor instance, so fmap away
leftArmIOHtml :: IO Html
leftArmIOHtml = renderHtml <$> leftArmIO


-- Q27.1
data Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box a) = Box (f a) 


morePresents :: Int -> Box a -> Box [a]
morePresents n box = fmap ((take n) . repeat) box 

-- Q27.2
myBox :: Box Int
myBox = Box 1

wrapped :: Box a -> Box (Box a)
wrapped box = fmap (\x -> Box x) box

unwrap :: Box a -> a
unwrap (Box a) = a

-- this allows you to do this
myWrappedBox = wrapped myBox -- => Box (Box 1)

-- fmap unwrap myWrappedBox => Box 1


































