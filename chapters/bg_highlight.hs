{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO -- needed for using IO with Text

dharma :: T.Text
dharma = "धर्"

bgText :: T.Text
bgText = " श्रेयान्स्वधर्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो" 

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
    where pieces = T.splitOn query fullText
          highlighted = mconcat ["{",query,"}"]

main :: IO ()
main = do
    TIO.putStrLn (highlight dharma bgText)
