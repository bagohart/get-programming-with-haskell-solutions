{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- not sure if according to the book, lazy or strict IO would be appropriate here.
-- Let's try both.
-- Lazy may have less memory requirements.
cpLazy :: String -> String -> IO ()
cpLazy src dest = do
    srcText <- readFile src
    appendFile dest srcText

-- There's no version that accepts Text as filename argument. lol? why? hm.
cpStrict :: String -> String -> IO ()
cpStrict src dest = do
    srcText <- TIO.readFile src
    TIO.appendFile dest srcText

