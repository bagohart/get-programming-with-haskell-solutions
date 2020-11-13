{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Exit
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines
-- there's also Text.Read, but it has a different interface

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    userInput <- TIO.getContents
    -- if T.head userInput == '\EOT' then exitSuccess else return () -- does this even do anything?
    let numbers = toInts userInput
    print (sum numbers)
