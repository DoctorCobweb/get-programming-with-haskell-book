-- programming in types
--
-- what does it mean to think about programming in types? 
-- well, types in haskell allow you to view programs as a 
-- series of transformations.
--
-- you can think of transformations as a more abstract
-- level of thinking about functions.
--
-- when solving problems in haskell you can approach
-- them first as a sequence of abstract transformations.
--
-- haskell allows you to combine types in ways not possible
-- in other languages. types can also take arguments of their
-- own.
--
-- after this lesson you'll be able to:
-- 1. understand `product` types in various programming languages
-- 2. use `sum` types to model problems in new ways
-- 3. think beyond hierarchical program design
--
-- most of the types you've seen so far are algebraic data types:
--
-- algebraic data types (ADT): are any types that can be made by 
-- combining other types.
--
-- the key to understanding ADTs is knowing exactly how to combine
-- other types. there are only 2 ways to do this:
-- 1. combine multiple types with an 'or': called 'sum types'.
--    e.ge. Bool type is True or False 
-- 2. combine multiple types with an 'and': called 'product types'.
--    e.g. Name type is a String and another String
--
--
------------------------
-- PRODUCT TYPES
------------------------
-- this is how you create an 'and' type
-- (using record-syntax)
data AuthorName = AuthorName {
      firstName :: String
    , lastName  :: String
    }

-- data Book = Book {
--       author :: AuthorName
--     , isbn   :: String
--     , title  :: String
--     , year   :: Int
--     , price  :: Double
--     }

-- most programming languages *only* allow for combining types
-- with an 'and' when making a new type.
-- making new types only by combining existing types leads to an interesting model
-- of designing software: you can only expand an idea by adding to it. you are 
-- constrained with top-down design; start with the most abstract representations
-- of a type that you can image, then add to it more detailed stuff to make more
-- specific 'objects' aka class hierarchies in OOP.
--
--
type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName
    | FirstNameWithTwoInits FirstName Char Char deriving (Show)

data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)
data Author = Author Name deriving (Show)
data Artist = Person Name | Band String deriving (Show)

hpLovecraft :: Creator
hpLovecraft = AuthorCreator
                (Author
                  (TwoInitialsWithLast 'H' 'P' "Lovecraft"))


madeBy :: StoreItem -> String
madeBy (BookItem (Book author _ _ _ _ ) )= show author 
madeBy (RecordItem record) = show (artist record)
--madeBy (BookItem book) = show (creator book) -- this also works like this
madeBy _ = "ibasdf"

-- putting together your bookstore
data Book = Book {
      author    :: Creator 
    , isbn      :: String
    , title     :: String
    , year      :: Int
    , bookPrice :: Double
    } deriving (Show)

data VinylRecord = VinylRecord {
      artist      :: Creator
    , recordTitle :: String
    , recordYear  :: Int
    , recordPrice :: Double
    } deriving (Show)

data CollectableToy = CollectableToy {
      name        :: String
    , description :: String
    , toyPrice    :: Double
    } deriving (Show)

data Pamphlet = Pamphlet {
      pamphTitle :: String
    , pamphDescription :: String
    , pamphContact :: String
    , pamphPrice :: Double} deriving (Show)

data StoreItem = BookItem Book 
    | RecordItem VinylRecord
    | ToyItem CollectableToy
    | PamphletItem Pamphlet deriving (Show)

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem vinyl) = recordPrice vinyl
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = 0.0 --pamplets are always free
