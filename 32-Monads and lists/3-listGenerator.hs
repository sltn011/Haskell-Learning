import Data.Char

powerOfTwo :: Int -> [Int]
powerOfTwo n = [2 ^ value | value <- [1 .. n]]

powerOfTwoAndThree :: Int -> [(Int, Int)]
powerOfTwoAndThree n = 
    [(pTwo, pThree)
    | value <- [1 .. n]
    , let pTwo = 2 ^ value
    , let pThree = 3 ^ value
    ]

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = [(oddVal, evenVal) | oddVal <- [1, 3 .. n], evenVal <- [2, 4 .. n]]

guardEven :: Int -> [Int]
guardEven n = [val | val <- [1 .. n], even val]

listColors :: [String]
listColors = ["brown", "blue", "pink", "orange"]

foo :: [String] -> [String]
foo strings = 
    [ mconcat ["Mr.", withCapital]
    | str <- strings
    , let withCapital = (toUpper (head str)) : (tail str)
    ]