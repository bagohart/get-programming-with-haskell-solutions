import System.Environment

main :: IO ()
main = do
    args <- getArgs
    traverse putStrLn args
    mapM putStrLn args
    mapM_ putStrLn args
    -- return ()

quickCheck221 :: IO ()
quickCheck221 = do
    -- replicateM 3 getLine -- would work too
    xs <- mapM (const getLine) [1..3]
    putStrLn "You entered the following things:"
    mapM_ putStrLn xs

-- quick check 22.2
replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n ma = mapM (const ma) [1..n]
