module Main where

import Lib
import System.IO
import System.Exit
import Control.DeepSeq

quickCheck223 :: IO ()
quickCheck223 = do
    putStrLn "Enter text to be reversed..."
    hSetBuffering stdin LineBuffering
    xs <- getContents
    putStrLn $ reverse xs
    putStrLn "end"

quickCheck224 :: IO ()
quickCheck224 = do
    putStrLn "Enter the things..."
    userInput <- getContents
    let numbers = toInts userInput
    numbers `deepseq` print (sum $ map (^2) numbers)

q222 :: IO ()
q222 = do
    putStrLn "I AM THE QUOTE MACHINE"
    hSetBuffering stdin LineBuffering
    userInput <- getContents
    let inputs = map toInputs . lines $ userInput
    mapM_ theAction inputs

theAction :: Input -> IO ()
theAction No = exitSuccess -- is there another way to terminate this lazy input thingy?
theAction (Quote n) = putStrLn $ quotes !! (n-1)

quotes :: [String]
quotes = [
           "dafuq"
         , "why"
         , "hm"
         , "this is not"
         , "convincing"
         ]

data Input = No | Quote Int deriving (Eq, Show)

-- this is not how you do programming :-)
toInputs :: String -> Input
toInputs "n" = No
toInputs "1" = Quote 1
toInputs "2" = Quote 2
toInputs "3" = Quote 3
toInputs "4" = Quote 4
toInputs "5" = Quote 5
toInputs _ = Quote 1 -- hm

-- main = quickCheck224
main = q222

-- main :: IO ()
-- main = do
--     putStrLn "Enter the things..."
--     -- seems to be a stack bug that ^D is treated as regular character. uh oh. or ghci. or readline. mysterious.
--     -- hSetBuffering stdin NoBuffering
--     hSetBuffering stdin LineBuffering
--     -- hSetBuffering stdin (BlockBuffering (Just 1))
--     userInput <- getContents
--     let numbers = toInts userInput
--     -- mapM_ print userInput
--     -- putStrLn ("The sum is " ++ show (sum numbers)) -- this does not behave as expected :)
--     numbers `deepseq` putStrLn ("The sum is " ++ show (sum numbers)) -- but this does. lol.

toInts :: String -> [Int]
toInts = map read . lines
