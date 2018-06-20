-- in FP languages you don't have state changes, and you also don't 
-- have common looping functions that rely on changing state such
-- as 'for', 'while', and 'until' loops.
-- this means that all iteration problems have to be solved through
-- recursion.
--
-- because thinking recursively is often headache-inducing, haskell
-- offers a feature called 'pattern matching' to make recursion 
-- much easier to reason with.
--
-- if you think of recursive functions as defining recursive processes,
-- recursion becomes fairly mundane.
--

-- myGCD a b = if remainder == 0
--             then b
--             else myGCD b remainder
--     where remainder = a `mod` b


-- using pattern matching we can simplify myGCD immensely
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)
