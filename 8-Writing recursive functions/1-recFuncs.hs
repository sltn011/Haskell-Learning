myLength [] = 0

myLength (_ : xs) =
    1 + myLength xs


myTake _ [] = []

myTake 0 _ = []

myTake n (x : xs) =
    x : rest
    where rest = myTake (n - 1) xs

finiteCycle (first : rest) =
    first : rest ++ [first]

myCycle (first : rest) =
    first : myCycle(rest ++ [first])