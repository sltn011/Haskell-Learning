ifEvenDo functor x =
    if even x
        then functor x
        else x

ifEvenInc x =
    ifEvenDo increment x

ifEvenDouble x =
    ifEvenDo double x

ifEvenSquare x =
    ifEvenDo square x

increment x =
    x + 1

double x =
    2 * x

square x =
    x ^ 2
        