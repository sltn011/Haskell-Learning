import Data.Array.Unboxed

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0, 9) [(5, True)] -- Остальные значения создаются по умолчанию

elemAt :: Int -> Bool
elemAt n =
    zeroIndexArray ! n

arrayWithFirstTrue :: UArray Int Bool
arrayWithFirstTrue = zeroIndexArray // [(0, True)]

filledWithZeros :: UArray Int Int
filledWithZeros = array (0, 5) $ zip [0 .. 5] $ cycle [0]

filledWithTwos :: UArray Int Int
filledWithTwos = accum (+) filledWithZeros $ zip [0 .. 5] $ cycle [2]