daysInMonths :: [Int]
daysInMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

generateNums :: [Int] -> [Int]
generateNums lastNums = [nums | n <- lastNums, nums <- [1 .. n]]

generateNumsDo :: [Int] -> [Int]
generateNumsDo lastNums = do
    n <- lastNums
    nums <- [1 .. n]
    return nums

generateNumsMonad :: [Int] -> [Int]
generateNumsMonad lastNums =
    lastNums >>=
        (\n -> [1 .. n] >>=
            (\nums -> return nums))