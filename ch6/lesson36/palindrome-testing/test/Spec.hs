import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char(isPunctuation,isSpace)
import Data.Text as T

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
    where noPuncText = T.filter (not . isPunctuation) text

-- exercise: more invariants
prop_capitalizationInvariant text = (T.toLower . preprocess) text == preprocess text
prop_whitespaceInvariant text = (Prelude.filter (not . isSpace) . T.unpack) text == (T.unpack . preprocess) text

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 1000} prop_punctuationInvariant
    putStrLn "done!"
