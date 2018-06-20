module Lib where

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
