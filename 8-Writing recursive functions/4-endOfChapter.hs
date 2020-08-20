myReverse [] = []

myReverse (x : xs) =
    (myReverse xs) ++ [x]

fib n =
    calcFib n 0 1

calcFib 0 _ b =
    b

calcFib n a b =
    calcFib (n - 1) b (a + b)