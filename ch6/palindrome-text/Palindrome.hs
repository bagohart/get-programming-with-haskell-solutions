module Palindrome(isPalindrome) where 

import qualified Data.Text as T
import Data.Char (isSpace,isPunctuation)

-- I can't find correspondig functions for Text, maybe because those work on single characters by design
-- still, this seems weird =/
-- looking at the source (the type of dropAround) it seems that there is a conversion to Char

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.pack . filter (not . isSpace) . T.unpack

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.pack . filter (not . isPunctuation) . T.unpack 

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess text
