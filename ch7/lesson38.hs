import Data.Char (isDigit)

oddList :: [Maybe Int]
oddList = Just <$> [1,3..]
-- ... what is this task about ?_?

-- q38.1

-- this task is rather stupid, because it doesn't really use ergonomic capacities
-- of Either. The solution is also wrong, it mixes left and right.
-- It seems a bit like Validation would be the appropriate type here, but
-- then I can't see which value couldn't be parsed, so no. lol.

addStrInts :: String -> String -> Either String Int
addStrInts n m
      | shouldWork n && shouldWork m = Right $ read n + read m
      | shouldWork n = Left "Second argument can't be parsed"
      | shouldWork m = Left "First argument can't be parsed"
      | otherwise = Left "Both arguments can't be parsed"


shouldWork :: String -> Bool
shouldWork = all isDigit

-- q38.2
-- actually, this should work without Bounded and Eq, but then I'd probabl have to catch exceptions -)
safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safeSucc x = if x == maxBound then Nothing else Just (succ x)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

safeLast :: [a] -> Either String a
safeLast [] = Left "This list is empty"
safeLast xs = safeLast' 100 xs

-- very reasonable function :D
safeLast' :: Int -> [a] -> Either String a
safeLast' _ [x] = Right x
safeLast' 0 (_:_:_) = Left "This list is too big, let's abort."
safeLast' n (_:xs) = safeLast' (n-1) xs
