-- things learned here:
-- 1. implement your own type classes
-- 2. understand polymorphism in haskell
-- 3. know when to use `deriving`
-- 4. search for documentation using Hackage and Hoogle


-- let's define a six-sided die
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

-- to make custom behaviour for SixSidedDie when using the `show`
-- function, you must implement this new type for the Show
-- typeclass.
instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

-- typeclasses are the way you use polymorphism in haskell.
--
--
-- let's implement Eq typeclass for SixSidedDie. for this you
-- need to provide details on 2 functions: 
-- 1. (==) :: a -> a -> Bool
-- 2. (/=) :: a -> a -> Bool
instance Eq SixSidedDie where
    (==) S1 S1 = True
    (==) S2 S2 = True
    (==) S3 S3 = True
    (==) S4 S4 = True
    (==) S5 S5 = True
    (==) S6 S6 = True
    (==) _ _ = False
    --(/=) = not (==)


instance Ord SixSidedDie where
    compare S6 S6 = EQ
    compare S6 _  = GT
    compare _  S6 = LT
    compare S5 S5 = EQ
    compare S5 _  = GT 
    compare _  S5 = LT 
    compare S4 S4 = EQ
    compare S4 _  = GT 
    compare _  S4 = LT
    compare S3 S3 = EQ
    compare S3 _  = GT
    compare _  S3 = LT
    compare S2 S2 = EQ 
    compare S2 _  = GT
    compare _  S2 = LT
    compare S1 S1 = EQ

instance Enum SixSidedDie where
    toEnum 0 = S1
    toEnum 1 = S2
    toEnum 2 = S3
    toEnum 3 = S4
    toEnum 4 = S5
    toEnum 5 = S6
    toEnum _ = error "No such value"

    fromEnum S1 = 0
    fromEnum S2 = 1
    fromEnum S3 = 2
    fromEnum S4 = 3
    fromEnum S5 = 4
    fromEnum S6 = 5

data Name = Name (String, String) deriving (Show, Eq)
names :: [Name]
names = [Name ("Emil","Cioran"),
         Name ("Eugene","Thacker"),
         Name ("Freidrich","Nietzsche")]

instance Ord Name where
    compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)




data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Ord, Eq, Show, Enum)

class (Eq a, Ord a, Enum a) => Die a where
    getSide :: a -> a

instance Die FiveSidedDie where
    getSide side = side

