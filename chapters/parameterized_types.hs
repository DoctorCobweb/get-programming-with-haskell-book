-- after this lesson you will be able to:
-- 1. use parameterized types to make generic types
-- 2. understand kinds of types
-- 3. write code using the Data.Map type to look up values
--
-- like functions, types can also take arguments. types take arguments by
-- using type variables the definitions. types defined using parameters
-- are called 'parameterized types'.
--
-- parameterized types are important in haskell because they enable you
-- to define generic data structures that work with a wide range of 
-- existing data.
--
-- the most basic parameterized type you could make is a `Box` that servers
-- as a container for any other type.
--
-- so Box type is an abstract container that can hold any other type.
-- as soon as you type a type inside Box, the Box type takes on a 
-- concrete value.
--
-- some importing stuff for later
import Data.Char
import qualified Data.Map as Map
--
data Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- Box is cool and all, but what's better is Triple. Check this out
data Triple a = Triple a a a deriving (Show)

-- Triple is NOT the same as a 3-tuple (a,b,c). Tuples in haskell can 
-- have different types as values. In this Triple type, all three values 
-- must be of the same type. This has many practical applications.
type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 42.2 34.6

-- people's names can be represented as a Triple of Strings
type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

-- can do this for initial of a person also
type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

-- since fst and snd only work for 2-Tuples we have to write
-- custom accessor function like this
first :: Triple a -> a
first (Triple x _ _ ) = x

second :: Triple a -> a
second (Triple _ x _ ) = x

third :: Triple a -> a
third (Triple _ _ x ) = x

-- easily turn Triple into a list
toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

-- this is the good part. we can write generic
-- functions to act on a Triple and still keep
-- it a Triple
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

transformGeneral :: (a -> b) -> Triple a -> Triple b
transformGeneral f (Triple x y z) = Triple (f x) (f y) (f z)

tripleMap :: (a -> b) -> [Triple a] -> [Triple b]
tripleMap f triples = map (transformGeneral f) triples

transformGeneralBox :: (a -> b) -> Box a -> Box b
transformGeneralBox f (Box x) = Box (f x)

boxMap :: (a -> b) -> [Box a] -> [Box b]
boxMap f boxes = map (transformGeneralBox f) boxes

-- lists (are special and get special treatement).
-- let's implement our own version of list
data List a = Empty | Cons a (List a) deriving Show

-- in words "a list of type `a` is either Empty of the
-- consing of the value `a` with another list of type `a`.
--
builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

-- let's implement map for our custom list type
ourMap :: (a -> b) -> List a -> List b
ourMap f Empty = Empty
ourMap f (Cons x rest) = Cons (f x) (ourMap f rest) 


----------------------------------------
-- kinds
----------------------------------------
-- the kind of a type indicates the number of parameters the type takes, which
-- are expressed using an asterisk (*).
--
-- types that take no parameters have a kind of *, types that take one parameter
-- have the kind * -> *, types with two parameters have the kind * -> * -> *
-- etc.
--
--
--
--
----------------------------------------
-- Data.Map
----------------------------------------
-- like dictionaries in many other languages
--
-- the type parameters of Map are the types of the keys
-- and values.
--
data Organ = Heart | Brain | Kidney | Spleen deriving (Show,Eq,Ord,Enum)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

-- the most common way to build a Map is with the fromList function
-- fromList :: Ord k => [(k,a)] -> Map k a

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

values = map snd (Map.toList organCatalog)
-- to look up an item in a Map.Map you use the Map.lookup function.
-- Map.lookup :: Ord k => Map k v -> Maybe v

allOrgans = [Heart .. Spleen]

-- modify the Organ type so it can be used as a key => deriving Ord class.
-- then build a Map called organInventory, of each organ to its count in 
-- organCatalog.

organCounts = map (\organ -> length ( filter (== organ) values)) allOrgans
organInventory = Map.fromList (zip allOrgans organCounts)

