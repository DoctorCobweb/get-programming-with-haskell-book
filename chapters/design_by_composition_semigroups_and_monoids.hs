-- after this lesson you can do:
-- 1. create new functions with function composition
-- 2. use `Semigroup` to mix colors
-- 3. learn how to use guards in code
-- 4. solve probability problems with `Monoid`
--
-- an important way in which haskell diverges from tradtional software design is with the idea of composability.
-- composability means that you create something new by combining two like-things.
--
-- haskell offers a standard way to combine instances of the same type together.
--
-- mathematical definitions (wikipedia 'Semigroup' article):
-- 1. semigroup : is an algebraid structure consisting of a set together with an associative binary operation.
--                the name "semigroup" comes from the fact that a semigroup generalizes a group by preserving 
--                only associativity and closure under the binary operation... a semigroup need not be commutative.
--
-- 2. monoid: is a semigroup with an identity element; thus obeying all but one of the axioms of a group (existence of
--            inverse elements). a natural example is strings with concatenation as the binary operation, and the empty
--            string being the identity element. if you were to do away with the empty string and just have non-empty
--            strings with concatenation, you would then have a semigroup, not a monoid.

-- we'll need these later. note, that haskell forces you to put
-- all imports at the top of the source file. throws an error otherwise.
import Data.List
import Data.Semigroup

--------------------------------------
-- 17.1 combining functions
--------------------------------------
-- look at how to combine functions before looking at combining arbitrary types.

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

--------------------------------------
-- 17.2 combing like-types: semigroups
--------------------------------------
-- before looking at the `Semigroup` typeclass we
-- need the  Data.Semigroup module (imported above)
--
-- Semigroup only requires 1 function to be implemented by 
-- a type in order for that type to be an instance of Semigroup.
-- that function is `<>` which is an 'operator' for combining
-- instances of the same type.
--
-- this is the type signature of <>:
-- (<>) :: Semigroup a => a -> a -> a 
-- you can take two like-types, combine them, and get a 
-- new thing of the same type.
--
-- here's how youd make Integer an instance of Semigroup:
instance Semigroup Integer where
    (<>) x y = x + y -- ie. define integer addition operator as our <> operator


-- combining colors as an example of semigroup usage.
data Color = Red
    | Yellow
    | Blue
    | Green
    | Purple
    | Orange
    | Brown 
    | White -- used as identity
    deriving (Show, Eq)

-- implement Semigroup for the Color type
-- instance Semigroup Color where 
--     (<>) Red Blue = Purple 
--     (<>) Blue Red = Purple 
--     (<>) Yellow Blue = Green
--     (<>) Blue Yellow = Green
--     (<>) Yellow Red = Orange 
--     (<>) Red Yellow = Orange 
--     (<>) a b = if a == b
--                then a
--                else Brown
    
-- as this instance stands, it doesnt support associativity. namely, we get different results
-- if we do this:
-- (Green <> Blue) <> Yellow
-- Brown
-- Green <> (Blue <> Yellow)
-- Green
--
-- to make associativity, use guards to be able todo computations on the arguments you're going to compare.
-- color fix is to say: if one color is used to make another, combining them yields the composite color.
-- purple + red = purple
-- green + yellow = green
-- green + blue = green
-- green + green = green
-- etc
--
instance Semigroup Color where 
    (<>) Red Blue = Purple 
    (<>) Blue Red = Purple 
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange 
    (<>) Red Yellow = Orange 
    (<>) Red White = Red
    (<>) Blue White = Blue
    (<>) Green White = Green
    (<>) Yellow White = Yellow
    (<>) Purple White = Purple
    (<>) Brown White = Brown
    (<>) White Red = Red
    (<>) White Blue = Blue
    (<>) White Green = Green
    (<>) White Yellow = Yellow
    (<>) White Purple = Purple
    (<>) White Brown = Brown
    (<>) White White = White
    (<>) a b | a == b = a
             | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
             | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
             | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
             | otherwise = Brown
    
instance Monoid Color where
    mempty = White
    mappend = (<>)
--------------------------------------------------
-- 17.3 composing with identity: monoids
--------------------------------------------------
--
-- before looking at the actual definition of `Monoid`, 
-- at face value one would think that `Monoid` should
-- be a subclass of `Semigroup`. that's because `Monoid`
-- is just `Semigroup` with `identity`, and so we'd expect
-- the definition to look something like this:
-- class Semigroup => Monoid a where
--     identity :: a
--
--
-- but! because `Monoid` predates `Semigroup`, haskell
-- has the following definition for `Monoid`
--
-- class Monoid a where
--     mempty :: a
--     mappend :: a -> a -> a
--     mconcat :: [a] -> a
--    
-- the only required definitions for `Monoid` are mempty and mappend.
-- once you provide them haskell is clever enough to make mconcat
--
-- mconcat :: Monoid a => [a] -> a
--
-- which means mconcat takes a list of Monoids and combines them,
-- returning a single Monoid.
--
-- Q: why the bother with having an identity in you possesion? 
--
-- A: well, it allows you to fold up a whole list of types. say you make a 
--    fold of strings and don't have the identity as your starting point.
--    then you'd always have some 'junk' string as you starting point
--    which is not in your list of strings. once folded, the combined
--    string will always have that 'junk' string present. if instead
--    you have an identity then your starting point can be the identity
--    and thus folding over the list of strings results in a large string
--    only containing that stuff that was in the list of strings; not 
--    
--    eg. without identiy "" aka a Semigroup
--    foldl func "yadda" ["blah", "blah"] ==> "yaddablahblah". see how
--    yadda hangs around in the result!
--
--    eg. with identity "" aka a Monoid
--    foldl func "" ["blah","blah"] ==> "blahblah".
--    the result only contains the stuff in the list, which is all you
--    may care about.
--
-- because of the identity for a Monoid, mconcat is defined using
-- foldr, mappend, and mempty:
--
-- mconcat = foldr mappend mempty
-- 
-- so this makes it easy to just to this (and get the results you kinda
-- expect to get):
--
-- mconcat ["does", " this", " make", " sense?"]
-- ==> "does this make sense?"
-- cool.
--
--
----------------------------------------
-- practical monoids - building probability tables
----------------------------------------
-- a list of Strings representing events and a list of
-- Doubles representing probabilities
type Events = [String]
type Probs = [Double]
--
-- a probability tables is a list of events paired
-- with a list of probabilities
data PTable = PTable Events Probs

-- make a function which creates a PTable, with 
-- normalized probabilities ie. the sum of all
-- probabilities sum to 1.
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where totalProbs = sum probs
          normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|", show prob,"\n"]

-- make PTable an instance of Show which means we need
-- to implement the show function.
instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

-- next, we want to be able to combine two, or more,
-- PTables. this happens when you have for example
-- 2 coins and you want to know the probabilities for all
-- possible outcomes. well you ask, what is good for combining types?
-- the Monoid typeclass exists for this very reason, so why
-- not try to make PTable type an instance of Monoid. then
-- you can use mconcat to combine PTables.
--
-- to achieve this you first need to make a function which 
-- gives you the 'cartesian product' of 2 lists. this just
-- means:
--   eg.1. you have 2 lists of Ints
--         l1 = [1,2,3]
--         l2 = [6,7,8]
--
--         then the cartesian product of l1 and l2 is
--         cartProd = [(1,6),(1,7),(1,8),
--                     (2,6),(2,7),(2,8),
--                     (3,6),(3,7),(3,8)
--                    ]
--
--  you can go further and define some function which acts on
--  each pair thus giving you more control on what exactly
--  the cartesian product should output.
--
--  define cartCombine which takes a function for combining two
--  lists, and two lists.
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where 
          repeatedTimes = length l2
          repeatedL1 =  map (take repeatedTimes . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2


-- now use cartCombine to combine events and probabilities
combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine eventCombiner e1 e2
    where eventCombiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

-- now let's make PTable an instance of Semigroup, then
-- Monoid. then we're done.
instance Semigroup PTable where
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable e1 p1) (PTable e2 p2) = PTable (combineEvents e1 e2) (combineProbs p1 p2) 

instance Monoid PTable where
    mempty = (PTable [] [])
    mappend = (<>)

-- let's play
coin :: PTable
coin = createPTable ["heads","tail"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]


