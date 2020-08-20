prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNum seed =
    (a * seed + b) `mod` maxNum

examplePRNG :: Int -> Int
examplePRNG = prng 13 7 111