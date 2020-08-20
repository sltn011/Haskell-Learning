import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

dataArray :: UArray Int Int
dataArray = listArray (0, 9) [1, 0, 2, 0, 3, 4, 5, 0, 6, 0]

replaceZeros :: Int -> UArray Int Int -> UArray Int Int
replaceZeros newVal arr = runSTUArray $ do
    stArr <- thaw arr
    let end = (snd . bounds) arr
    forM_ [0 .. end] $ \i -> do
        val <- readArray stArr i
        when (val == 0) $ do
            writeArray stArr i newVal
    return stArr