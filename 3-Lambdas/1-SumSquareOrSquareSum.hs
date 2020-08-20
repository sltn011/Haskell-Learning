sumSquareOrSquareSum_v1 x y =
    if sumSquare > squareSum
        then sumSquare
        else squareSum
    where sumSquare = x ^ 2 + y ^ 2
          squareSum = (x + y) ^ 2

sumSquareOrSquareSum_v2 x y =
    (\sumSquare squareSum ->
        if sumSquare > squareSum
        then sumSquare
        else squareSum) (x ^ 2 + y ^ 2) ((x + y) ^ 2)

sumSquareOrSquareSum_v3 x y =
    let sumSquare = (x ^ 2 + y ^ 2)
        squareSum = (x + y) ^ 2
    in
        if sumSquare > squareSum
            then sumSquare
            else squareSum 