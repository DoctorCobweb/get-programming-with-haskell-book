-- using functions in a context
--
-- things learned here
-- 1. build an application that handles missing data
-- 2. *extend* the power of the Functor typeclass with the Applicative type
-- 3. use Applicative to use 1 data model in many contexts
-- the Applicative typeclass extends the power of Functor typeclass by allowing
-- you to use functions that are theselves in a context.
--
-- it allows you to chain together long sequences of computation in a context
-- such as IO or Maybe.

import qualified Data.Map as Map

type LatLong = (Double,Double)

-- lat/long for some cities
locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]


-- since we're on a sphere, to calculate the distance between
-- two cities we need to use the haversine formula
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat, long) = (rlat, rlong)
    where rlat = toRadians lat
          rlong = toRadians long


haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where (rlat1,rlong1) = latLongToRads coords1
          (rlat2,rlong2) = latLongToRads coords2
          dlat = rlat2 - rlat1
          dlong = rlong2 - rlong1
          a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
          c = 2 * atan2 (sqrt a) (sqrt (1-a))
          earthRadius = 3961.0


haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)


printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")


-- you want a generalization of Functor's fmap. one that takes multiple arguments
-- (more than fmap's 2 arguments)
--
-- the real limitations of <$> is that you end up with a function in a context.
-- and because of partial application you have no way of using that function
-- (because every function in haskell is seen as only taking a single argument)

-- check this out
--
--   maybeInc :: Maybe (Integer -> Integer)
--   maybeInc = (+) <$> Just 1 
--
-- so its a function in a context because the (+) operator takes 2 values.
-- and now we've created a function waiting for a missing value, but it's inside
-- a Maybe. and furthermore, now you have a Maybe fuction but there's no way to
-- apply this function.
--
-- to alleviate this problem we introduce the <*> operator
--
-- (<*>) :: Applicative f => f (a->b) -> f a -> f b
--
-- this allows you to do this
--
-- maybeInc <*> Just 5          -- => Just 8
-- maybeInc <*> Nothing         -- => Nothing
--
-- now we have a general way to use existing binary functions in a Maybe context
--
-- (++) <$> Just "cats" <*> Just " and dogs"     -- => Just "cats and dogs"
-- (++) <$> Nothing <*> Just " and dogs"         -- => Nothing
-- (++) <$> Just "cats <*> Nothing               -- => Nothing
--
--
-- quick check 28.3
-- (*) <$> val1 <*> val2
-- `div` <$> val1 <*> val2
-- `mod` <$> val1 <*> val2





-- Maybe LatLong -> Maybe LatLong -> Maybe Doublt

-- haversine LatLong -> LatLong -> Double
city1Maybe = Map.lookup "Carcosa" locationDB
city2Maybe = Map.lookup "New York" locationDB

distanceBetweenCities = haversine <$> city1Maybe <*> city2Maybe




-- aside
-- this program handles missing values well, but not once did we have
-- to check whether a value was null by using conditionals, or worry
-- about exception handling.
--
-- even better, the core functionality of the program, haversine function,
-- could be written as though nothing in the world may go wrong (here that
-- is the querying of a nonexistent city in bd).
--
-- haskell makes these errors impossible to creep into your programming.
main :: IO ()
main = do

    putStrLn "enter city 1 names"
    city1 <- getLine
    putStrLn "enter city 2 names"
    city2 <- getLine

    let city1Maybe = Map.lookup city1 locationDB
    let city2Maybe = Map.lookup city2 locationDB

    let distanceMaybe = haversine <$> city1Maybe <*> city2Maybe

    printDistance distanceMaybe





----------------------------------------
-- SUMMARY QUESTIONS
----------------------------------------
-- Q 28.1
haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO latLong1 latLong2 = do 
    val1 <- latLong1
    val2 <- latLong2
    return (haversine val1 val2)
    

-- Q 28.2
haversineIO' :: IO LatLong -> IO LatLong -> IO Double
haversineIO' latLong1 latLong2 = haversine <$> latLong1 <*> latLong2 
    
    









