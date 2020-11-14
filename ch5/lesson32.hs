import Control.Monad
import Control.Applicative

-- list of squares of overy odd number < 20
considerThis :: [Int]
considerThis = [x^2 | x <- [1..20], odd x]

considerThis' :: [Int]
considerThis' = do
    x <- [1..20]
    guard $ odd x
    return $ x^2

-- not sure if this is what the tasks wants me to do
qc321 :: [(Int,Int)]
qc321 = do
    x <- [1..10]
    y <- map (^2) [1..10]
    return (x,y)

-- ... or this
qc321' :: [(Int,Int)]
qc321' = do
    x <- [1..10]
    return (x,x^2)

-- this is filter
qc322 :: (a -> Bool) -> [a] -> [a]
qc322 f xs = do
    x <- xs
    guard $ f x
    return x

-- this doesn't actually look that complicated. hm. then again, 3 days ago, this function seemed completely magical to me u_U
guard' :: Alternative f => Bool -> f ()
guard' True = pure ()
guard' False = empty

-- this is stupid
evenNumbers :: [Int]
evenNumbers = 0 : do
    (+2) <$> evenNumbers

-- hm
lol :: [Int]
lol = do
    x <- [1..5]
    [x]

-- q32.1
calenderDates = [ show d ++ "." ++ (months !! (m-1)) | m <- [1..12], d <- [1..days !! (m-1)]]

-- is this even correct? :D
days :: [Int]
days = [31,28,31,30,31,30,30,31,30,31,30,31]

months :: [String]
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

-- q32.2
calenderDates' = do
    m <- [1..12]
    d <- [1..days !! (m-1)]
    return $ show d ++ "." ++ (months !! (m-1)) 
    
calenderDates'' = [1..12] >>=
    (\m -> [1..days !! (m-1)] >>=
        (\d -> [show d ++ "." ++ (months !! (m-1))]))
