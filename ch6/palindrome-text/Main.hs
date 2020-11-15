{-# LANGUAGE OverloadedStrings #-}

module Main where

import Palindrome
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "Enter a word and I'll let you know if it's a palindrooooooome!"
    text <- TIO.getLine
    let response = if isPalindrome text
                      then "it is =D"
                      else "it's not :'("
    TIO.putStrLn response

