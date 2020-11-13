import System.IO

-- 2nd main
-- main :: IO ()
-- main = do
--     helloFile <- openFile "hello.txt" ReadMode
--     firstLine <- hGetLine helloFile
--     putStrLn firstLine
--     secondLine <- hGetLine helloFile
--     goodbyeFile <- openFile "goodbye.txt" WriteMode
--     hPutStrLn goodbyeFile secondLine
--     hClose helloFile
--     hClose goodbyeFile
--     putStrLn "done!"


main :: IO ()
main = do
    helloFile <- openFile "hello.txt" ReadMode
    isEOF <- hIsEOF helloFile
    firstLine <- if not isEOF
                    then hGetLine helloFile
                    else return "empty"
    putStrLn firstLine
    -- quick check 24.2
    secondLine <- if not isEOF
                     then hGetLine helloFile
                     else return ""
    if not $ null secondLine
       then do
           goodbyeFile <- openFile "goodbye.txt" WriteMode
           hPutStrLn goodbyeFile secondLine
           hClose goodbyeFile
        else return ()
    putStrLn "done!"
    
