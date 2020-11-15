-- I moved the OverloadedStrings extension to the "extension" field in the .cabal file
-- but then stack screams at me that it's deprecated :)
-- the internet is not sure if this feature is even fundamentally a good idea.
-- uh oh.

module Main where

import Palindrome
import Data.Text as T
import Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome!"
    text <- TIO.getLine
    let response = if isPalindrome text
                    then "it is!"
                    else "it's not!"
    TIO.putStrLn response
