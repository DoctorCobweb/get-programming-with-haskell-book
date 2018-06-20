{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- things learned
-- 1. use the `Text` type for more efficient text processing
-- 2. change haskell's behaviour with language extensions
-- 3. program using common text functions
-- 4. use `Text` to properly handle Unicode text
--
-- the preferred type for working with text data is `Text` type.
-- String is implemented like a linked-list.
--
-- Text 
-- 1. is implemented as an array under the hood.
-- 2. does NOT use lazy evaluation
--
-- to use Data.Text you need to use the 'text'
-- dependency, as seeen in build dependencies
-- in chapter.cabal file

-- Data.Text has two functions which can be used to conver
-- 1. String -> Text , and has type T.pack :: String -> Text
-- 2. Text -> String , and has type T.unpack :: Text -> String

import qualified Data.Text as T
import Data.Semigroup




firstWord :: String
firstWord = "persistent"

secondWord :: T.Text
secondWord = T.pack firstWord 

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

-- 23.2.1 OverloadedStrings and haskell extensions
-- an annoying thing about T.Text is that this code throws an error
myWord :: T.Text
myWord = "dog"

-- that's because the literal "dog" is a String. to fix this you need to change
-- the way GHC actually works aka reads in source files. the fix is to use 
-- language extensions. here, the specific extension we're going to use to fix the "dog"
-- problem, is called OverloadedStrings
-- 
-- there are 2 ways to load extensions
-- 1. when compiling with GHC:
--    $ stack ghc text.hs -XOverloadedStrings (note no space between -X and Overloaded...)
--    or 
--    $ ghci -XOverloadedStrings
-- 2. (preferred method) use a LANGUAGE pragma, which looks like:
--    {-# LANGUAGE <Extension Name> #-}
--    then just go about your haskell day like normal:
--    $ stack ghci

-- other useful language extensions
-- ViewPatterns : allows for more sophisticated pattern matching
-- TemplateHaskell : provides tools for haskell meta programming
-- DuplicateRecordFields : solves the annoying problem where the same
--                         field name for different types using record
--                         syntax causes a conflict
-- NoImplicitPrelude : some haskell programmers prefer a custom Prelude.
--                     this language extension allows you to not use the
--                     default Prelude


--------------------------------------------------
-- 23.2.2 basic text utilities
--------------------------------------------------
sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- T.lines sampleInput
-- => ["this","is","input"]

someText :: T.Text
someText = "Some\ntext for\t you"

-- T.words someText
-- => ["Some","text","for","you"]

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "this is a simple to do"

-- T.splitOn breakText exampleText
-- => ["this is a ", " to do"]


combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"


myTLines = T.splitOn "\n"

myTUnlines = T.intercalate "\n"



