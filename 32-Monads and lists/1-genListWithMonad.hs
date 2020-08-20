powersOfTwo :: Integer -> [Integer]
powersOfTwo numElems = do
    value <- [1 .. numElems]
    return (2 ^ value)

powersOfTwoAndThree :: Integer -> [(Integer, Integer)]
powersOfTwoAndThree numElems = do
    value <- [1 .. numElems]
    let pTwo = 2 ^ value
    let pThree = 3 ^ value
    return (pTwo, pThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds numElems = do
    evenVal <- [2, 4 .. numElems]
    oddVal <- [1, 3 .. numElems]
    return (oddVal, evenVal)

valAndSquare :: Integer -> [(Integer, Integer)]
valAndSquare numElems = do
    val <- [1 .. numElems]
    return (val, val^2)