import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef


aLargeList :: [Int]
aLargeList = [1..10000000]

aLargeArray :: UArray Int Int
aLargeArray = array (0,9999999) []

aLargeListDoubled :: [Int]
aLargeListDoubled = map (*2) aLargeList

aTestArray :: UArray Int Int
aTestArray = array (5,10) []
-- aTestArray = array (5,10) [(5,1337),(4,-1)] -- nope

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1..10] $ cycle [True]

-- qc 42.1
qcArray :: UArray Int Bool
qcArray = array (0,4) [(2,True),(3,True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) []

-- qc 42.2
beansInBuckets' :: UArray Int Int
beansInBuckets' = array (0,3) $ zip [0..3] $ repeat 0 -- not sure why the solution always uses cycle =?

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1,5),(3,6)]

twoBeansExtra :: UArray Int Int
twoBeansExtra = accum (+) updatedBiB $ zip [0..3] $ cycle [2]

-- qc 42.3
doubleAllBeans :: UArray Int Int
doubleAllBeans = accum (\e _ -> 2*e) twoBeansExtra $ zip [0..3] $ cycle [undefined]

-- solution :: UArray Int Int
-- solution = accum (*) updatedBiB $ zip [0 .. 3] $ cycle [3]
-- ^ this seems to be... not an actual answer to the question?

-- this doesn't work. :'(
-- doubleAllBeans' :: UArray Int Int
-- doubleAllBeans' = (*2) <$> twoBeansExtra

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0..end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

listToUArray2 :: [Int] -> UArray Int Int
listToUArray2 vals = runSTUArray $ do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0..end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

-- more general ST magic
-- I'm not convinced that this is actually useful :)
swapST :: (Int,Int) -> (Int,Int)
swapST (x,y) = runST $ do
    x' <- newSTRef x
    y' <- newSTRef y
    writeSTRef x' y -- y is the value, x' is the mutable... thingy
    writeSTRef y' x
    xfinal <- readSTRef x'
    yfinal <- readSTRef y'
    -- writeSTRef x' 1337 -- activating this line does NOT break the code x)
    return (xfinal,yfinal)
