-- run with stack ghci --package random bytestring
-- or build stack project with dependencies

import System.Environment
import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad

-- -- first draft of main
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let fileName = head args
--     imageFile <- BC.readFile fileName
--     -- glitched <- return imageFile
--     let glitched = imageFile -- qc. 25.2
--     let glitchedFileName = mconcat ["glitched_", fileName]
--     BC.writeFile glitchedFileName glitched
--     print "all done"

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255 -- couldn't this be 256?

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
    where (before, rest) = BC.splitAt loc bytes
          after = BC.drop 1 rest
          newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1,bytesLength)
    charVal <- randomRIO (0,255)
    return (replaceByte location charVal bytes)

-- -- second draft of main, change a single byte
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let fileName = head args
--     imageFile <- BC.readFile fileName
--     glitched <- randomreplaceByte imageFile
--     let glitchedFileName = mconcat ["glitched_", fileName]
--     BC.writeFile glitchedFileName glitched
--     print "all done"

-- random char
qc253 :: IO Char
qc253 = do
    c <- randomRIO (0,255)
    return $ intToChar c

-- or just
qc253' :: IO Char
qc253' = randomRIO (minBound :: Char, maxBound :: Char)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
    where (before,rest) = BC.splitAt start bytes
          (target,after) = BC.splitAt size rest
          changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    start <- randomRIO (0, (BC.length bytes) - sectionSize)
    return (sortSection start sectionSize bytes)

-- -- third draft of main, change a section
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let fileName = head args
--     imageFile <- BC.readFile fileName
--     glitched <- randomSortSection imageFile
--     let glitchedFileName = mconcat ["glitched_", fileName]
--     BC.writeFile glitchedFileName glitched
--     print "all done"

-- -- fourth draft of main, both steps repeated
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let fileName = head args
--     imageFile <- BC.readFile fileName
--     glitched1 <- randomReplaceByte imageFile
--     glitched2 <- randomSortSection glitched1
--     glitched3 <- randomReplaceByte glitched2
--     glitched4 <- randomSortSection glitched3
--     glitched5 <- randomReplaceByte glitched4
--     let glitchedFileName = mconcat ["glitched_", fileName]
--     BC.writeFile glitchedFileName glitched5
--     print "all done"


-- let's foldM this thing. (why not State it actually?)

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- foldM (\bytes func -> func bytes) imageFile
                                                    [
                                                        randomReplaceByte
                                                      , randomSortSection
                                                      , randomReplaceByte
                                                      , randomSortSection
                                                      , randomReplaceByte
                                                    ]
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    print "all done"
    
-- this code seems to be buggy because sometimes feh won't read it. okular complains, too. I don't get why though.
-- maybe I really failed just to copy it o_O

-- qc 24.4
glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [
                                                        randomReplaceByte
                                                      , randomSortSection
                                                      , randomReplaceByte
                                                      , randomSortSection
                                                      , randomReplaceByte
                                                    ]

-- q25.2 seems boring, it's like sortSection but without the sort, it even contains the 'reverse' already
