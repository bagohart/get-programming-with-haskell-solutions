module Main where
-- ^ this is implicitly assumed if this line is missing!
-- says the book. except it seems not to be true because now I can't compile it any more
-- if the main action is missing =/

main = undefined

-- head :: [a] -> a
-- head (x:_) = x
-- head [] = errorEmptyList "head"

head :: Monoid a => [a] -> a
head (x:_) = x
head [] = mempty

example :: [[Int]]
example = []

-- qc 34.1
length :: Int
length = 8
-- use Main.length to refer to it
