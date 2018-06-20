{-# LANGUAGE OverloadedStrings #-} -- use string literals for all string types



-- learn
-- 1. unique binary format used by libraries
-- 2. write tools to bulk-process binary data by using ByteString
-- 3. working with Unicode data by using the Text type
-- 4. structuring a large program performing complicated I/O tasks
--
-- this program with use data on books, that are created by libraries,
-- to make a HTML document.
--
-- book data is stored in an obscure MARC format. MARC records are in
-- a binary format which also make heavy use of Unicode to properly
-- store character encodings.
--
-- steps
-- 1. create a type for the book data
-- 2. learn how MARC records are formatted
-- 3. take a batch of serialized MARC records from a single file,
--    then convert them into a list of individual records
-- 4. parse individual files to find the information you need
-- 5. process all MARC records into HTML files


import qualified Data.ByteString as B -- need a way to manipulate bytes
import qualified Data.Text as T
import qualified Data.Text.IO as TIO -- IO functions for Text
import qualified Data.Text.Encoding as E -- safely work with Unicode.
import Data.Maybe

-- when dealing with Unicode data, the best way is to use ByteStrings 
-- for manipulating bytes, and Text for everything else.


-- type synonyms
type Html = T.Text -- final type will be Html
type Author = T.Text
type Title = T.Text
type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString
type FieldText = T.Text


-- the Book type will be the product type of Author and Title
data Book = Book {
    author :: Author
  , title :: Title } deriving Show


-- each field metadata in the directory has the same 3 fields
-- so make a data type to reflect this.
data FieldMetadata = FieldMetadata { tag :: T.Text -- first 3 bytes
                                   , fieldLength :: Int -- next 4 bytes
                                   , fieldStart :: Int -- rest of bytes
                                   } deriving Show


-- the first 24 bytes of a record is defined to be the leader
leaderLength :: Int
leaderLength = 24

-- subfields of a field are separated using a delimiter, the
-- ASCII character number 31.
fieldDelimiter :: Char
fieldDelimiter = toEnum 31


-- tags and subfields for title and author.
-- e.g. to fetch title: field 245 and subfield 'a' (main title)
-- e.g. to get author: field 100 and subfield 'a'
titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'


-- each field metadata appearing in the directory is 12 bytes.
-- so the directory length should always be an integer multiple
-- of 12.
dirEntryLength :: Int
dirEntryLength = 12


book1 :: Book
book1 = Book {
    title = "The Conspiracy Against the Human Race"
   ,author = "Ligotti, Thomas"
   }
book2 :: Book
book2 = Book {
    title = "A Short History of Decay"
   ,author = "Cioran, Emil"
   }
book3 :: Book
book3 = Book {
     title = "The Tears of Eros"
    ,author = "Bataille, Georges"
    }


myBooks :: [Book]
myBooks = [book1,book2,book3]


bookToHtml :: Book -> Html
bookToHtml book = mconcat [ "<p>\n"
                          , titleInTags
                          , authorInTags
                          , "</p>\n"
                          ]
    where titleInTags = mconcat ["<strong>", (title book), "</strong>\n"]
          authorInTags = mconcat ["<em>", (author book), "</em>\n"]


booksToHtml :: [Book] -> Html
booksToHtml books = mconcat [ "<html>\n"
                            , "<head><title>books</title>"
                            , "<meta charset='utf-8'/>"
                            , "</head>\n"
                            , "<body>\n"
                            , booksHtml
                            , "\n</body>\n"
                            , "</html>"
                            ]
    where booksHtml = (mconcat . (map bookToHtml)) books




getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record


rawToInt :: B.ByteString -> Int
rawToInt bs = read (T.unpack (E.decodeUtf8  bs)) 


-- the first 5 bytes of the leader contains a number
-- telling you the length of the record.
getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)


-- consider your file as a ByteString. take this ByteString
-- and separate it into a pair of values: the first record
-- and the rest of the remaining ByteString.
nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest bs = B.splitAt recordLength bs
    where recordLength = getRecordLength (getLeader bs)


-- to iterate through the entire file, recursively use nextAndRest
-- to take a record and rest of the file. put the record in a list,
-- then repeat on the rest of the file until you reach the end
allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords bs | bs == B.empty = []
              | otherwise = next: allRecords rest
    where (next, rest) = nextAndRest bs


getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
    where remainder = B.drop 12 leader


getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)


getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = directory
    where leader = getLeader record 
          noLeader = B.drop leaderLength record
          (directory, rest) = B.splitAt (getDirectoryLength leader) noLeader


splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory bytes | bytes == B.empty = []
                     | otherwise = field : splitDirectory rest
    where (field, rest) = B.splitAt dirEntryLength bytes


makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata { tag = textTag
                                        , fieldLength = theLength
                                        , fieldStart = theStart
                                        }
    where (theTag, afterTag) = B.splitAt 3 entry
          textTag = E.decodeUtf8 theTag
          (theFieldLength, theFieldStart) = B.splitAt 4 afterTag
          theLength = rawToInt theFieldLength
          theStart = rawToInt theFieldStart


getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata rawEntries = map makeFieldMetadata rawEntries


getFieldText :: MarcRecordRaw -> FieldMetadata -> FieldText
getFieldText rawRecord fieldMetadata = E.decodeUtf8 byteStringValue
    where baseAddress = getBaseAddress rawRecord
          baseRecord = B.drop baseAddress rawRecord
          baseAtField = B.drop (fieldStart fieldMetadata) baseRecord
          byteStringValue = B.take (fieldLength fieldMetadata) baseAtField


-- safely looking up FieldMetadata from the directory
lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if length results < 1
                                  then Nothing
                                  else Just (head results)
    where metadata = (getFieldMetadata . splitDirectory . getDirectory) record
          results = filter ((== aTag) . tag) metadata


-- safely looking up a potentially missing subfield
lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just fieldMeta) subfield record = if results == []
                                                      then Nothing
                                                      else Just ((T.drop 1 . head) results)
    where fieldText = getFieldText record fieldMeta
          subfields = T.split (== fieldDelimiter) fieldText
          results = filter ((== subfield) . T.head) subfields


-- general lookupValue function for looking up 
-- tage-subfield pairs
lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
    where entryMetadata = lookupFieldMetadata aTag record


-- make some helper functions for looking up author and title
lookupTitle :: MarcRecordRaw -> Maybe T.Text
lookupTitle record = lookupValue titleTag titleSubfield record

          
lookupAuthor :: MarcRecordRaw -> Maybe T.Text
lookupAuthor record = lookupValue authorTag authorSubfield record


-- putting it all together. finally.
--
-- take in a MARC file and return a list of pairs
marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marc = zip titles authors
    where records = allRecords marc
          titles = map lookupTitle records 
          authors = map lookupAuthor records


-- change all these Maybe pairs into a list of Book type.
-- to do this, you only create a Book when both Author and
-- Title are Just values.
-- to help achieve this, use the fromJust function from
-- Data.Maybe module.
pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book {title = fromJust title, author = fromJust author}) justPairs 
    where justPairs = filter (\(title, author) -> isJust title && isJust author) pairs


processRecords :: Int -> B.ByteString -> Html
processRecords n bytes = (booksToHtml . pairsToBooks . (take n) . marcToPairs) bytes


main :: IO ()
main = do
    marcData <- B.readFile "sample.mrc"
    let n = 10000
    let nRecordsProcessed = processRecords n marcData
    TIO.writeFile "books.html" nRecordsProcessed
    print (processRecords n marcData)



















