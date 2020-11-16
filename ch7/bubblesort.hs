import Data.Traversable
import Data.Foldable

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
-- import Data.STRef


myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]

-- qc 42.4
myData' :: UArray Int Int
myData' = listToUArray [7,6,4,8,10,2]

myOtherData' :: UArray Int Int
myOtherData' = listArray (5,8) [4,3,2,1]

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

-- this works on zero-based things. I think. hm.
bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1..end] $ \i -> do
        forM_ [0..(end-i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j+1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j nextVal
                writeArray stArray (j+1) val
    return stArray

testTheFor :: IO ()
testTheFor = do
    forM_ [1..5] $ \i -> print i

testTheFor' :: IO ()
testTheFor' = do
    for_ [1..5] $ \i -> print i

testTheFor'' :: IO ()
testTheFor'' = for_ [1..5] print

lol = for_ [1..5] print

rofl = for [1..5] print

-- q42.1
a1 :: UArray Int Bool
a1 = listArray (0,4) [True,True,True,True,True]

a2 :: UArray Int Bool
a2 = listArray (0,4) [False,False,False,False,False]


-- I think this should actually yield 2 arrays as a result, but the task wants me to compute only 1. hm.
-- I'll just swap the values in the two arrays, this seems more useful
-- otherwise I'd probably want to create one new array, which would be fine, too, but then why the state thing?

-- assume that both arrays are zero based and have the same length
-- checking would be easy, but verbose

-- type GeneticArray = UArray Int Bool
-- crossOver :: Int -> (GeneticArray,GeneticArray) -> (GeneticArray,GeneticArray)
-- ^ ok, this won't just work, because runSTUArray expects a single array to be returned, and this is not that!!!

crossOver :: Int -> UArray Int Bool -> UArray Int Bool -> UArray Int Bool
crossOver cutOff a1 a2 = runSTUArray $ do
    stArray1 <- (thaw a1) -- What is the type here? I didn't find out ='(
    let end = (snd . bounds) a1
    forM_ [cutOff..end] $ \i ->
        writeArray stArray1 i (a2 ! i)
    return (stArray1)

-- q42.2
replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros arr = runSTUArray $ do
    stArray <- thaw arr
    let end = (snd . bounds) arr
    forM_ [0..end] $ \i -> do
        val <- readArray stArray i
        when (val == 0) $
            writeArray stArray i (-1)
    return stArray
-- ^ this works. is it really fast? maybe. hm.

testData :: UArray Int Int
testData = listArray (0,5) [1,0,1,7,0,0]

