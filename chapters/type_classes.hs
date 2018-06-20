-- typeclasses
--
-- 1. understance the basics of type classes
-- 2. be able to read type class definitions
-- 3. use common type classes: Num, Show, Eq, Ord, and Bounded.
--
-- typeclasses allow you to group types based on shared behaviour.
--
-- at first glance, typeclasses look similar to interfaces in OOP languages.
--
-- a typeclass states which functions a type must support, in the same
-- way an interface specifies which methods a class must suport.
--
-- haskell's use of typeclasses, in a way, forces you to think in increasingly
-- more powerful forms of abstraction. in other words, typeclasses abstract 
-- patterns and provide ways of working with typed data.
--
-- typeclasses are in many ways at the heart of haskell programming.
--
-- GHCi> :t (+)
-- (+) :: Num a => a -> a -> a
--
-- in words this says: "there's some type `a` of class Num"
-- Num is a typeclass that generalizes the idea of a number.
--
--
--
-- any type that's an instance of `Describable` will have a `describe` function, which when
-- called will tell you about the type ie. produce some string documenting the type.
class Describable a where
    describe :: a -> String



-- haskell defaults to the order of the data constructors when you
-- derive the `Ord` typeclass here.
data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)


-- this function works just like `succ` on Bounded types
-- but can be called an unlimited number of times without error.
-- it will work for any types which are instances of 
-- Bounded, Enum, and Ord
cycleSucc :: (Bounded a, Enum a, Ord a) => a -> a
cycleSucc n = if n >= maxBound
              then maxBound
              else succ n

