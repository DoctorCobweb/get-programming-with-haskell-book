-- after this lesson you'll be able to
-- 1. understand the `Maybe` type
-- 2. use the `Maybe` type to handle missing values
-- 3. design programs with `Maybe` types
-- 
-- Maybe types represent values that might be missing. This type
-- represents a context for a value.
--
-- most other languages represent a missing value by the null value.
-- by stipulating the context that a value might be missing, the 
-- Maybe type allows one to write much safer code.
--
-- => errors related to null values are systematically removed
-- from haskell programs.
--
import qualified Data.Map as Map
import Data.List

data Organ = Heart | Brain | Kidney | Spleen deriving (Show,Eq) 
organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- using Maybe as a solution to missing values
-- Maybe is used in all the typical places the Null values pop up:
-- 1. opening files that may or may not exist
-- 2. reading from a database that could have null values
-- 3. making RESTful API requests to a potentially missing resource
--

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = (\id -> Map.lookup id catalog)


availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter 
                                      (\x -> x == Just organ) 
                                      available)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan Nothing = ""
showOrgan (Just organ) = show organ

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

-- quick check 19.2
numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just x) = x


-- back to the lab - more complext computation with Maybe

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart 
organToContainer Spleen = Bag Spleen
organToContainer Kidney = Bag Kidney

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat a) = (Lab,Vat a)
placeInLocation (Cooler a) = (Lab,Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location,Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location,Container) -> String
report (location, container) = show container ++ 
                               " in the " ++
                               show location

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
    where organ = Map.lookup id catalog

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"





-- end of chapter questions

-- Q 19.1
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers contents = (length . filter (not . isSomething)) contents

-- Q 19.2
maybeFunction :: (a -> b) -> Maybe a -> Maybe b
maybeFunction f Nothing = Nothing
maybeFunction f (Just a) = Just (f a)

maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap f listOfMaybes = map (maybeFunction f )listOfMaybes










