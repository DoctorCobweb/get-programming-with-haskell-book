module Main where

import Palindrome 
-- import qualified Data.Text.IO as TIO
import qualified Data.Text.IO as TIO


-- CONVENTION: in haskell, modules should live in files of the 
-- same name as the module.
-- => this filename is Main.hs so we called the module in thise
-- file Main, also


main :: IO ()
main = do
    print "enter a word to check if it's a palindrome"
    text <- TIO.getLine
    let response = if isPalindrome text
                   then "it IS"
                   else "not is is NOT"
    print response
