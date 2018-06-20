module Primes where


primes :: [Int]
primes = sieve [2 .. 10 ]


sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime: sieve noFactors
    where noFactors = filter (not . ( == 0) . (`mod` nextPrime) ) rest


isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | n >= length primes = Nothing
          | otherwise = Just (n `elem` primes) 

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = [] 
unsafePrimeFactors n (next:primes) = if n `mod` next == 0
                                     then next: unsafePrimeFactors (n `div` next) (next:primes)
                                     else unsafePrimeFactors n primes


primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= length primes = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
    where primesLessThanN = filter ( <= n) primes


------------------------------------------------------------
-- 38.3.1 building a prime checker with Either
------------------------------------------------------------
isPrimeEither :: Int -> Either PrimeError Bool
isPrimeEither n
    | n < 2 = Left InvalidValue
    | n > length primes = Left TooLarge
    | otherwise = Right (n `elem` primes)
    

-- instead of always using String for the Left value constructor's type, 
-- further customize what the error can mean by defining our own data type for 
-- errors wrt our prime functions
data PrimeError = TooLarge | InvalidValue


-- now make it an instance of show
instance Show PrimeError where
    show TooLarge = "Value exceeded max bound"
    show InvalidValue = "value is not a valid candidate for prime checking"


displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "it is a PRIME"
displayResult (Right False) = "it is a composite num"
displayResult (Left primeError) = show primeError





