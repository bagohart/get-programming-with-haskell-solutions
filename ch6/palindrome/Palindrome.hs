module Palindrome(isPalindrome) where 
-- ^ file name should be Palindrome.hs, and this module only exports the function isPalindrome.
-- If I write it like this:
-- module Palindrome where 
-- then ALL functions will be exported!

-- qc 34.2
-- module Palindrome(isPalindrome
--                  , preprocess
--     ) where 

import Data.Char (toLower,isSpace,isPunctuation)
-- ^ import 3 functions from Data.Char by their original names

stripWhiteSpace :: String -> String
stripWhiteSpace = filter (not . isSpace)

stripPunctuation :: String -> String
stripPunctuation = filter (not . isPunctuation)

toLowerCase :: String -> String
toLowerCase = map toLower

preprocess :: String -> String
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
    where cleanText = preprocess text
