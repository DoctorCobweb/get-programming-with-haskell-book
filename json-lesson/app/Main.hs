module Main where

import Lib
import Data.Aeson -- aeson was the father of the ancient greek mythical hero jason
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad
import Data.Maybe


-- the great thing about aeson is that it lets you
-- apply haskell's strong type system to JSON data (which
-- pays little regard to data types, it mostly just 
-- has strings)

-- aeson relies on two straightforward functions todo the bulk
-- of the work.
-- 1. decode: take JSON data and transform into a target type
-- 2. encode: takes a type that's an instance of ToJSON, and
--            returns a JSON object represented as a ByteString.


data Book = Book { title :: T.Text
                 , author :: T.Text
                 , year :: Int
                 } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author="Will Jurt"
              , title="Learn Haskell"
              , year=2017
              }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON 

eitherBookFromWrongJSON :: Either String Book
eitherBookFromWrongJSON = eitherDecode wrongJSON


rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Will Kurt\",\"title\":\"Learn Haskell\",\"year\"=2017}"

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Cioran\",\"title\": \"A Short History of Decay\",\"year\":1949}" 

data Name = Name { firstName :: T.Text
                 , lastName :: T.Text
                 }

------------------------------------------------------------
-- 40.3.2 writing your own instances of FromJSON and ToJSON
------------------------------------------------------------
-- to make your type an instance of FromJSON manually, you
-- need to define one function: parseJSON



sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"


data ErrorMessage = ErrorMessage { message :: T.Text
                                 , errorCode :: Int
                                 } deriving Show


-- the operator (.:) takes an Object (our JSON object) and some
-- text, and returns a value parsed in a context
--
-- (.:) :: FromJSON a => Object -> Text -> Parser a
--
-- so
--     v .: "message"
--
-- is trying to parse the message field from your JSON object.
-- the result is a value in a Parser context. the reason you
-- need a context for your parse is that it can fail if 
-- there's trouble parsing.
instance FromJSON ErrorMessage where
    parseJSON (Object v) = ErrorMessage <$> v .: "message"
                                        <*> v .: "error"

instance FromJSON Name where
    parseJSON (Object v) = Name <$> v .: "firstName"
                                <*> v .: "lastName"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError


-- the object function creates our JSON object
-- the (.=) operator is used to create a key/value
-- pair matching the value of your data with the 
-- field name for the JSON data.
instance ToJSON ErrorMessage where
    toJSON (ErrorMessage messageBlah errorCodeBlah) =
        object [ "message" .= messageBlah
               , "error" .= errorCodeBlah
               ]

instance ToJSON Name where
    toJSON (Name firstNameBlah lastNameBlah) =
        object [ "firstName" .= firstNameBlah
               , "lastName" .= lastNameBlah
               ]

sampleErrorMessageJSON :: BC.ByteString
sampleErrorMessageJSON = encode sampleErrorMessage

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "everything is a oh kayy" 0

------------------------------------------------------------
-- 40.4 putting it all together: reading your NOAA data
------------------------------------------------------------
-- going to model the data.json response (got from last lesson)
-- using a custom data type:
--
--
--
--                -----> Metadata --> Resultset
--              /
-- NOAAResponse 
--              \
--                -----> NOAAResults
--
--
data NOAAResult = NOAAResult { uid :: T.Text
                             , mindate :: T.Text
                             , maxdate :: T.Text
                             , name :: T.Text
                             , datacoverage :: Double
                             , resultId :: T.Text
                             } deriving Show

-- because the data uses id instead of resultId, we will have to create
-- our own instance of FromJSON.
instance FromJSON NOAAResult where
    parseJSON (Object v) = NOAAResult <$> v .: "uid"
                                      <*> v .: "mindate"
                                      <*> v .: "maxdate"
                                      <*> v .: "name"
                                      <*> v .: "datacoverage"
                                      <*> v .: "id"

-- we wont be too concerned with ToJSON because we are only
-- reading from the data

-- next. need to create the Metadata type.
-- the first part of your Metadata is Resultset.
-- thankfully we don't have to create our own instance of FromJSON
-- because none of the fields conflict with any of haskell's function names

data Resultset = Resultset { offset :: Int
                           , count :: Int
                           , limit :: Int
                           } deriving (Show, Generic)

data Metadata = Metadata { resultset :: Resultset
                         } deriving (Show, Generic)

-- finally create our NOAAResponse type
data NOAAResponse = NOAAResponse { metadata:: Metadata
                                 , results :: [NOAAResult] 
                                 } deriving (Show, Generic)

instance FromJSON Metadata 
instance FromJSON Resultset 
instance FromJSON NOAAResponse


----------------------------------------------------------------------
-- summary question 40.1 - make NOAAResponse type an instance of ToJSON.
----------------------------------------------------------------------
instance ToJSON Metadata where
    toJSON (Metadata metadataBlah) =
        object [ "metadata" .= metadataBlah]

instance ToJSON Resultset where
    toJSON (Resultset offsetBlah countBlah limitBlah) =
        object [ "offest" .= offsetBlah
               , "count" .= countBlah
               , "limit" .= limitBlah ]

instance ToJSON NOAAResult where
    toJSON (NOAAResult uidBlah mindateBlah maxdateBlah nameBlah coverageBlah resultsIdBlah) =
        object [ "uid" .= uidBlah
               , "mindate" .= mindateBlah
               , "maxdate" .= maxdateBlah 
               , "name" .= nameBlah 
               , "coverage" .= coverageBlah 
               , "id" .= resultsIdBlah 
               ]

instance ToJSON NOAAResponse where
    toJSON (NOAAResponse metadataBlah resultsBlah) =
        object [ "metadata" .= metadataBlah
               , "results" .= resultsBlah]
----------------------------------------------------------------------


-- our goal is to print out all the types in the file.
-- to do this we need to define a printResults IO action
-- also, because we have a Maybe type, we need to handle the
-- case of a parse failing.
--
-- we will use forM_ to loop through our results and print them
printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just ourResults) = do
    forM_ ourResults (print . name) 

checkEither :: Either String NOAAResponse -> IO ()
checkEither (Left val) = print val
checkEither (Right val) = print "yadda noaaresponse good"


main :: IO ()
main = do
    jsonData <- B.readFile "data.json"


    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults = results <$> noaaResponse

    -- EXTENSION STUFF
    -- this stuff will user eitherDecode, which helps
    -- in debugging if anything goes wrong this the parsing.
    -- let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
    -- checkEither noaaResponse

    -- playing around with printing the encoded version
    -- of noaaResponse
    -- => have to use another do-block because of 
    --    Maybe type stuff. i think.
    if isJust noaaResponse
    then do
        let recoded = encode (fromJust noaaResponse)
        print recoded
    else print "NO DICE. encoding failed."

    -- back to the book code
    printResults noaaResults
