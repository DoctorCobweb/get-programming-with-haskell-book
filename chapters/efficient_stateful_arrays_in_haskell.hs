-- things learned
-- 1. use the `UArray` type for efficient storage and retrieval.
--    the U stands for unboxed.
-- 2. perform stateful computation on arrays with `STUArray`
-- 3. treat properly encapsulated, staeful functions as pure funcs
--
-- at first glance, the following problem seems impossible in haskell:
-- Q. efficient, inplace array algorithms.
--
-- to address this problem, and solve the issue, we will be led to 
-- consider
-- 1. strict (nonlazy) array type `UArray`
-- 2. finding a context for performing mutation on an array by
--    using the STUArray type
--
-- as an example, we will implement a bubble sory algorithm. and
-- the code using the above new types will run much faster than
-- if one were to use lists.
--

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

----------------------------------------------------
-- 42.1 creating effiecient arrays in haskell with the UArray type
----------------------------------------------------
-- we are facing 3 efficiency problems for the bubble sort challenge:
-- 1. lists are inherently slower than arrays for operations that involve
--    looking up values
-- 2. lazy evaulations can cause major performance problems
-- 3. in-place sorting requires mutation (statefult programming).
--
-- UArray type solves 1. and 2.
--

aLargeList :: [Int]
aLargeList = [1 .. 10000000]

-- unboxed arrays dont use lazy evaluation. they
-- use strict evaluation.
aLargeArray :: UArray Int Int
aLargeArray = array (0,9999999) []

-- to investigate the performance issues of lazy evaluation, lets look
-- at a modified version of the aLargeList
aLargeListDoubled :: [Int]
aLargeListDoubled = map (*2) aLargeList

-- getting the length of this list takes 1.58 secs. it's due to how
-- lazy evaluation works:
-- haskell stores all of the needed computations for creating the 
-- defined list in memory. computations such as those needed to generate
-- the list in the first place, and then computations to mulptiply 
-- the list by 2. there is a terminology for these to-be-needed-later
-- computations, 'thunks'.
--
-- so, when you have a large list needing to be created 'on the fly' when
-- a piece of code needs it (eg. calling length on the list), all these
-- thunks have to be evaluated. this leads to long waiting times.
--
-- by demanding strict evaluation we remove the use of thunks. UArray 
-- does just this, but there's a catch:
--
--     unboxed arrays such as UArray, only work with primative types
--     like Int, Char, Bool, Double.
--
-- (apparently haskell also has the 'Array' type which will work 
--  with any data type and acts like a list, but it is LAZY)
--
-- when creating a UArray you get to chose what the indices are (!).
-- => can use types that are members of Enum and Bounded
-- => types like Char, Int.
--

-- this will create a zero-index array with all
-- values set to False, except for index 3 value,
-- which will be True.
zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]

-- book's terse way
qcArray :: UArray Int Bool
qcArray = array (0,4) $ [(2, True), (3, True)]

-- dre's naive way
-- qcArray :: UArray Int Bool
-- qcArray = array (0,4) $ mconcat [ [(0,False), (1,False)]
--                                 , [(2,True),(3,True)]
--                                 , [(4,False)]
--                                 ]


beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) $ zip [0 .. 3] $ cycle [0]

-- updating values at an indices is done using the '//' operator
updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1,5), (3,6)]

-- here's how to add two to every element
-- accum (+) updatedBiB $ zip [0 .. 3] $ cycle [2]
--
-- with UArray, we get efficient lookups (using ! operator) and
-- a more efficient data structure in general (updating elems etc)
--
-- the next big issue to tack is the 'inplace' nature of sorting algorithms.
-- haskell by default will not change data once its been created; you need
-- to create a copy of the older data with all updates done to it. think copies.
-- note: UArray updating as show above works but is still making copies
--
-- so how are we to mimic/make inplace sorting?



--------------------------------------------------
-- 42.2 mutating state with STUArray
--------------------------------------------------
-- STUArray is an instance of Monad which means we can do computations
-- with a context.
--
-- STUArray uses a more general type called ST.ST
--
-- the key power that STUArray offers is the ability to change values in UArray
--
-- STUArray exists to allow you to perform stateful programming only when
-- statefulnemss is _indistinguishable_ from pure code for the users of your
-- funcs.
--
-- let's make a func which takes a list and returns a STUArray version of it
listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

-- taking values our of an STUArray is done with the following func.
-- we get the best of both worlds; stateful code in a safe context & 
-- still treat it as pure code.
-- runSTUArray :: ST s (STUArray s i e) -> UArray i e

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

-- most of the time we embed listToSTUArray func into our listToArray func
listToUArray' :: [Int] -> UArray Int Int
listToUArray' vals = runSTUArray $ do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray


--------------------------------------------------
-- 42.4 implementing a bubble sort
--------------------------------------------------
myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]
    

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do

    -- make UArray into ST s STUArray. 
    -- <- then takes it out of context, giving a STUArray
    stArray <- thaw myArray 

    -- bounds func gives you a pair representing the bounds
    -- of the array
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i -> do
      forM_ [ 0 .. (end -i)] $ \j -> do
        val <- readArray stArray j
        nextVal <- readArray stArray (j + 1)
        let outOfOrder = val > nextVal
        when outOfOrder $ do
          writeArray stArray j nextVal
          writeArray stArray (j+1) val
    return stArray



--------------------------------------------------
-- summary questions
--------------------------------------------------
-- Q42.1
gene1 :: UArray Int Int
gene1 = listArray (0,5) [1,1,1,1,1]

gene2 :: UArray Int Int
gene2 = listArray (0,5) [0,0,0,0,0]

crossover :: Int -> UArray Int Int -> UArray Int Int -> UArray Int Int
crossover crossPt g1 g2 = runSTUArray $ do
    g1Array <- thaw g1
    -- g2Array <- thaw g2 -- doing this causes an error
    let g2End = (snd . bounds) g2
    forM_ [crossPt .. g2End] $ \i -> do
      let yadda =  g2 ! i
      writeArray g1Array i yadda
    return g1Array

