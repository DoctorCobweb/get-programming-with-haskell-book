module Main where

import Primes

main :: IO ()
main = do
    print "enter a number to test for primality"
    number <- read <$> getLine
    let result = isPrimeEither number
    print (displayResult result)
