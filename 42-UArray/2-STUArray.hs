import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
    let end = length vals - 1
    stArray <- newArray (0, end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray stArray i val
    return stArray
