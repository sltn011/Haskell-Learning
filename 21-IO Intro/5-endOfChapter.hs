fibNum :: Integer -> Integer
fibNum 0 = 0
fibNum 1 = 1
fibNum n =
    if n < 0
        then -1
        else calcFib n 0 1

calcFib :: Integer -> Integer -> Integer -> Integer
calcFib 0 res _ = res
calcFib n a b = calcFib (n - 1) b (a + b)

foo :: IO Integer
foo = do
    inp <- getLine
    let nthFibNum = fibNum (read inp)
    return nthFibNum

bar :: IO String
bar = do
    inp <- getLine
    let num = fibNum (read inp)
    return (show num)