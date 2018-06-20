module Pizza where


-- this program will as for the cost amd size of 2 pizzas, and
-- will tell you which one is the better deal in terms
-- of cost per square inch

import qualified Data.Map as Map

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size/2)^2

type Pizza = (Double,Double)

costPerInch :: Pizza -> Double
costPerInch (size,cost) = cost / areaGivenDiameter size

-- returns the cheapest pizza out of the two inputed
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if p1Cost <= p2Cost
                      then p1
                      else p2
    where p1Cost = costPerInch p1
          p2Cost = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) = "the cheaper pizza has size " ++ 
                             show size ++ 
                             " , costs " ++
                             show cost ++
                             " cost per square inch: " ++
                             show costSqInch
    where costSqInch = costPerInch (size,cost)



-- 21.3.1 a peek at monad -- do-notation in Maybe
--
-- IO can use do-notation because its a member of a 
-- powerful type class called `Monad`.
--
-- do-notation has nothing to to with IO in particular,
-- and can be used by any member of `Monad` to perform
-- computation in a context.
--
-- the context for values in a `Maybe` is that they 
-- may not exist.
--
-- the context for IO is that you're interacting with
-- real world and your data might not behave as it does
-- in the rest of the haskell program you've written.
--
-- `Maybe` is also a member of the `Monad` typeclass,
-- and therefore can also use the do-notation.
--
-- check this out.
--
costData :: Map.Map Int Double
costData = Map.fromList [(1,10.0),(2,16.5)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.4)]


-- the new thing here is the addition of the
-- `return` function at the end of the
-- do-block. it takes a value of a type
-- and puts it back in the context of the
-- do-notation. here, that means we
-- get Maybe String type from the return
-- function.
maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData
    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

-- the `Monad` typeclass allows you to write general programs
-- that can work in a wide variety of contexts.
--
-- unlike the `Maybe` type, you can never remove values from
-- an `IO` type.
--
-- haskell has a special do-notation that allows you to write
-- code as though you weren't in the context of an `IO` type
--
--
-- Q21.1 
--

helloPerson :: String -> String
helloPerson name = "hello " ++ name ++ "!"

personNames :: Map.Map Int String 
personNames = Map.fromList [(1,"Yadaa"),(2,"Blahhhh")]

maybeHelloPerson ::Int -> Maybe String
maybeHelloPerson pId = do
    pName <- Map.lookup pId personNames
    let statement = helloPerson pName
    return statement

