import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

dataArray :: UArray Int Int
dataArray = listArray (0, 9) [6, 8, 1, 3, 9, 2, 7, 4, 10, 0]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort arr = runSTUArray $ do
    stArray <- thaw arr
    let end = (snd . bounds) arr
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - 1)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j + 1)
            when (val > nextVal) $ do
                writeArray stArray j nextVal
                writeArray stArray (j + 1) val
    return stArray