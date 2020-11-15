module Main where -- file should be called Main.hs

-- import qualified Palindrome
import Palindrome
-- now you can refer to the methods in Palindrome via e.g. Palindrome.isPalindrome

-- isPalindrome :: String -> Bool
-- isPalindrome text = text == reverse text

main :: IO ()
main = do
    print "Enter a word and I'll let you know if it's a palindrooooooome!"
    text <- getLine
    -- let response = if isPalindrome text
    -- let response = if Palindrome.isPalindrome text
    let response = if isPalindrome text
                      then "it is =D"
                      else "it's not :'("
    print response

