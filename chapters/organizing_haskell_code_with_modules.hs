module Main where

-- things learned
-- 1. understand the `main` module implicitly used when you create a program
-- 2. create namespaces for your functions using modules
-- 3. separate programs into multiple files
-- 4. selectively import functions from modules
--
-- creating namespaces for your functions is one of the most basic topics in programming.
-- haskell uses a system of modules to create separate namespaces.
--
-- `Prelude` is the standard modules for haskell. it has many commonly used functions 
-- definied in it. Prelude is automatically imported.



head :: Monoid a => [a] -> a
head (x:xs) = x
head [] = mempty

example :: [[Int]]
example = []

length :: Int
length = 9


-- if you name your module Main, as we have done here, haskell
-- will complain that you havn't provided a main function.
-- to fix this, either chnage the module name to something other than Main,
-- or just make a blank main function as done below
main :: IO ()
main = return () 
