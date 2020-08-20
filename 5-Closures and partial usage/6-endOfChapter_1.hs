ifEvenDo functor x =
    if even x
        then functor x
        else x

ifEvenInc = ifEvenDo ((+) 1)

ifEvenDouble = ifEvenDo ((*) 2)

ifEvenSquare = ifEvenDo (flip (^) 2)