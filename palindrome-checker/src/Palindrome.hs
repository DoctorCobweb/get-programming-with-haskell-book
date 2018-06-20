--{-# LANGUAGE OverloadedStrings #-}
module Palindrome  where

import qualified Data.Text as T
import Data.Char ( toLower
                 , isSpace
                 , isPunctuation
                 ) 


stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.filter (not . isSpace) text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

toLowerCase :: T.Text -> T.Text
toLowerCase text = T.map toLower text

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess text


-- hijacking palindrome-checker project to use for chaper 38
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n-1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM _ [] = []
myTakePM n (x:xs) = x : myTakePM (n-1) xs

myHead :: [a] -> a
myHead [] = error "asdfasdf blah empty list"
myHead (x:_) = x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

-- this is NOT totally safe because we're using 'tail' function
-- which is also a partial function. so we've fixed the 'head' 
-- but not the 'tail'.
myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer _ Nothing = Nothing
myTakeSafer 0 _ = Just [] 
myTakeSafer n (Just vals) = (:) <$> maybeHead vals
                                <*> myTakeSafer (n-1) (Just (tail vals))



-- the Either type
eitherHead :: [a] -> Either String a
eitherHead [] = Left "there is no head because the list is empty"
eitherHead (x:xs) = Right x

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "car"

charExampleEmpty :: [Char]
charExampleEmpty = ""


