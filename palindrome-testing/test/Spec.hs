import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified Data.Text as T
import Data.Char ( isPunctuation
                 , isSpace
                 , toLower
                 )


prop_punctuationInvariant text = preprocess text == preprocess noPuncText
    where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

prop_whiteSpaceInvariant text = stripWhiteSpace text == noWhiteSpace
    where noWhiteSpace = T.filter (not . isSpace) text

prop_caseInvariant text = toLowerCase text == lowerCaseText
    where lowerCaseText = T.map toLower text

main :: IO ()
main = do 
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_reverseInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_whiteSpaceInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_caseInvariant
    putStrLn "\ndone testing"

