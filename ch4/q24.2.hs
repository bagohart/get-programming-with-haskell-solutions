{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- read a file and rewrite it as capitalized
-- needs strict IO because otherwise I have to read and write the file at the same time.
-- Though maybe this is possible with ReadWriteMode? Let's see...
capitalizeStrict :: String -> IO ()
capitalizeStrict src = do
    content <- TIO.readFile src
    TIO.writeFile src (T.toUpper content)

-- can this work lazy?
capitalizeLazy :: String -> IO ()
capitalizeLazy src = do
    handle <- openFile src ReadWriteMode
    content <- hGetContents handle
    hPrint handle (map C.toUpper content)
-- this approach does not work because hPrint wipes the file and hGetContents closes the handle I think. lol
-- Apparently, hGetContents can be modified to not close the handle though: o_O

hGetContents' :: Handle -> IO String
hGetContents' h = do
    eof <- hIsEOF h
    if eof
       then return []
       else do
           c <- hGetChar h
           fmap (c:) (hGetContents' h)

-- this works almost, but it writes the uppercased things at the end instead of replacing the text
capitalizeLazy' :: String -> IO ()
capitalizeLazy' src = do
    handle <- openFile src ReadWriteMode
    content <- hGetContents' handle
    hPrint handle (map C.toUpper content)

-- this works. I'm not really sure what this is doing (-:
capitalizeLazy'' :: String -> IO ()
capitalizeLazy'' src = do
    handle <- openFile src ReadWriteMode
    content <- hGetContents' handle
    hSeek handle AbsoluteSeek 0
    hPutStr handle (map C.toUpper content)
    hClose handle
    return ()
