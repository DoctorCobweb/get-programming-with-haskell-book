-- in this capstone 
-- 1. learn the basics of timeseries analysis
-- 2. combining multiple timeseries with Monoid and Semigroup
-- 3. using Map to solve problems of duplicate values in timeseries
-- 4. avoiding errors involving missing values by using Maybe
--
import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

-- dummy data
file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]

-- you can solve the problem of stitching together individual timeseries 
-- by making your timeseries an instance of Semigroup, then use the
-- (<>) :: a -> a -> a
--
-- if you also want to combine a list of timeseries, then you will 
-- need to make your timeseries an instance of Monoid, thus giving
-- you the use of mconcat :: [a] -> a
--
-- 
----------------------------------------
-- 20.1.1 building a basic timeseries type
----------------------------------------
data TS a = TS [Int] [Maybe a]


-- this is a lot worse than what textbook version of
-- createTS is, learning still. it does the same
-- thing though.
sortByInts :: (Int,a) -> (Int,a) -> Ordering
sortByInts (x,_) (y,_) = if x < y
                         then LT
                         else if x > y
                              then GT 
                              else EQ 

checkForJusts :: Int -> Map.Map Int a -> Maybe a
checkForJusts id catalog = if isNothing value
                        then Nothing
                        else value
    where value = Map.lookup id catalog


createTS :: [Int] -> [a] -> TS a
createTS times values = TS wholeTimePeriod maybeValues
    where givenData = zip times values

          sortedTimes = sortBy sortByInts givenData
          wholeTimePeriod = [minTime .. maxTime]
          minTime = fst (head sortedTimes)
          maxTime = fst (last sortedTimes)

          allNothings = take (length wholeTimePeriod) (cycle [Nothing])
          zippedNothings = zip wholeTimePeriod allNothings

          catalogOfData = Map.fromList givenData

          maybeValues = map (\ (id, _) -> checkForJusts id catalogOfData) zippedNothings


-- given a file which is [(Int,a)] you want to make a TS a
fileToTS :: [(Int,a)] -> TS a
fileToTS tvPairs = createTS times values
    where (times, values) = unzip tvPairs

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just val) = mconcat [show time, "|", show val, "\n"]
showTVPair time Nothing = mconcat [show time, "|", "NA\n"]

instance Show a => Show (TS a) where
    show (TS times vals) = mconcat rows
        where rows = zipWith showTVPair times vals


-- can finally convert all files to TS types
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

-- now we want to use Semigroup and Monoid somehow..
timeseriesToZip :: TS a -> [(Int,Maybe a)]
timeseriesToZip (TS times vals) = zip times vals 

updateMap :: Ord k =>  Map.Map k v -> (k, Maybe v) -> Map.Map k v
updateMap theMap (k, Nothing) = theMap
updateMap theMap (k, (Just p)) = Map.insert k p theMap

-- important to note that this function:
-- 1. the second timeseries variable will overwrite fields of the 
--    first timeseries if their ids match
-- 2. the combination of the 2 timeseries stretches over the union
--    of the periods of each timeseries.
combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where bothTimes = mconcat [t1, t2]
          completeTimes = [minimum bothTimes .. maximum bothTimes]
          ts1Map = foldl updateMap Map.empty (zip t1 v1)
          updatedMap = foldl updateMap ts1Map (zip t2 v2)
          combinedValues = map (\t -> Map.lookup t updatedMap) completeTimes
          
-- now we can make (TS a) an instance of Semigroup
instance Semigroup (TS a) where
    (<>) = combineTS

instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1,ts2,ts3,ts4]

-- although it took a load of work to get to tsAll, in future, should we ever
-- want to add more timeseries data, all we have to write is mconcat [ts1, blahb, blah]




-- 20.3 performing calculations on the timeseries
mean :: Real a => [a] -> Double
mean vals = realToFrac (sum vals) / realToFrac (length vals) 

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all ( == Nothing) values
                              then Nothing
                              else Just avg
    where justVals = filter isJust values
          bareVals = map fromJust justVals
          avg = mean bareVals


type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
    where newFunc (i1,Nothing) (i2,Nothing) = (i1,Nothing) 
          newFunc (_,Nothing) (i2,v2) = (i2,v2)
          newFunc (i1,v1) (_,Nothing) = (i1,v1)
          newFunc (i1, Just v1) (i2, Just v2) = if func v1 v2 == v1
                                                then (i1, Just v1)
                                                else (i2, Just v2)


-- can now compare all values in a TS type
compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) = if all (== Nothing) values
                                   then Nothing
                                   else Just yadda
    where pairs = zip times values
          yadda = foldl (makeTSCompare func) (0, Nothing) pairs

-- let's make some TSCompareFunc functions
minTS :: Ord a => TS a -> Maybe (Int,Maybe a)
minTS timeseries = compareTS min timeseries

maxTS :: Ord a => TS a -> Maybe (Int,Maybe a)
maxTS timeseries = compareTS max timeseries


-- 20.4 transforming timeseries

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair _ Nothing = Nothing
diffPair Nothing _ = Nothing
diffPair (Just x) (Just y) = Just (x -y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues)
    where shiftValues = tail values
          diffValues = zipWith diffPair shiftValues values


meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals = if any (==Nothing) vals
                 then Nothing
                 else Just ( mean (map fromJust vals))



-- my attempt at it. compiles and works but doesnt care about edge cases.
-- movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
-- movingAvg [] _ = []
-- movingAvg vals n = if length vals < n
--                    then []
--                    else movingAvgList
--     where movingAvgList = map meanMaybe (buildList n vals) 


-- buildList :: Int -> [Maybe a] -> [[Maybe a]]
-- buildList n [] = []
-- buildList n vals = take n vals : buildList n (tail vals)


-- movingAverageTS :: (Real a) => TS a -> Int -> TS Double
-- movingAverageTS (TS times []) window = TS times []
-- movingAverageTS (TS times vals) window = TS times mAvg
--     where mAvg = movingAvg vals window


movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n = if length nextVals == n
                   then (meanMaybe nextVals): movingAvg restVals n
                   else []
    where nextVals = take n vals
          restVals = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n = TS [] []
movingAverageTS (TS times values) n = TS times finalVals
    where finalVals = mconcat [nothings, mAvg, nothings]
          nothings = replicate (n `div` 2) Nothing
          mAvg = movingAvg values n










