{-#LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LazyT
import qualified Data.Text.Lazy.IO as LazyTIO

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["hello", " ", name, "!"]

main :: IO ()
main = do
    TIO.putStrLn "what is ur name"
    name <- TIO.getLine
    let statement = helloPerson name
    TIO.putStrLn statement


toInts :: LazyT.Text -> [Int]
toInts = map (read . LazyT.unpack) . LazyT.lines 

blah :: IO ()
blah = do
    userInput <- LazyTIO.getContents
    let nums = toInts userInput
    LazyTIO.putStrLn ((LazyT.pack . show . sum) nums)
