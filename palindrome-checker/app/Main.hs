--{-# LANGUAGE OverloadedStrings #-}

module Main where

import Palindrome
--import Data.Text as T
import Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "type in a word to check if it's a palindrome.."
    text <- TIO.getLine
    let result = if isPalindrome text
                 then "YES it is a palindrome"
                 else "NOT a palindrome"
    TIO.putStrLn result
