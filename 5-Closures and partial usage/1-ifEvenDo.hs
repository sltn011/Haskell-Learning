ifEvenDo functor x =
    if even x
        then functor x
        else x

genIfEven func = (\x -> ifEvenDo func x)

ifEvenInc = genIfEven (\x -> x + 1)

ifEvenDouble = genIfEven (\x -> x * 2)

ifEvenSquare = genIfEven (\x -> x ^ 2)
        