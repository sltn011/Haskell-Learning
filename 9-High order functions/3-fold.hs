myFoldl _ accum [] =
    accum

myFoldl func accum (x : xs) =
    myFoldl func newAccum xs
        where newAccum = accum `func` x

--cons это то же самое что и (:), cons x xs => x : xs
--rcons это конс с обратным порядком аргументов

rcons xs x =
    x : xs

foldlReverse xs = 
    myFoldl rcons [] xs

--foldl (-) 0 [1, 2, 3] = (((0 - 1) - 2) - 3)
--foldr (-) 0 [1, 2, 3] = (1 - (2 - (3 - 0)))

myFoldr _ accum [] =
    accum

myFoldr func accum (x : xs) =
    x `func` rightVal
        where rightVal = myFoldr func accum xs