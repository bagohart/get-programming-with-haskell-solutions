allFmapA :: Applicative m => (a -> b) -> m a -> m b
allFmapA f ma = pure f <*> ma

-- q30.1
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f ma = do
    a <- ma
    return $ f a

-- q30.2
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mab ma = do
    fab <- mab
    a <- ma
    return $ fab a
