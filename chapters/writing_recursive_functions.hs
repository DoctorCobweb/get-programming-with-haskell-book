-- writing recursive functions
--
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:rest
    where rest = myTake (n-1) xs

myCycle (first:rest) = first:myCycle (rest ++ [first])

-- ackermann function
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

-- collatz conjecture
collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n*3 + 1)


-- i dont think you can pattern match lists like this...
-- because they may be infinite
-- myReverse [] = []
-- myReverse (xs:x) = [x] ++ (myReverse xs)

rev1 [] = []
rev1 (x:xs) = rev1 xs ++ [x]

