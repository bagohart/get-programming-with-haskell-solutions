main :: IO ()
main = do
    userInput <- getContents
    mapM_ print userInput

quickCheck223 :: IO ()
quickCheck223 = do
    xs <- getContents
    putStrLn $ reverse xs
