-- in haskell, one could say that the fundamental data structure is a list.
--
-- basics to be learned are
-- 1) taking lists apart
-- 2) putting them back together
-- 3) essential functions on lists
-- 4) lazy evaluation- allows you to represent infinitely long lists
--
-- lists are a great prototype structure to use when thinking about more
-- complicated concepts later on, like monads, functors and applicative functors.
--
-- a list is either an empty list or an element followed by another list.
-- taking apart and building lists are fundamental tools for many
-- techniques in FP.
--
--

assignToGroups n aList = zip groups aList
    where groups = cycle [1..n]


-- all list functions, including those automatically included in the Prelude module, are in
-- the Data.List module.

inFirstHalf theList x  = if elem x (take halfLength theList)
                         then True
                         else False
    where halfLength = div (length theList)  2 
