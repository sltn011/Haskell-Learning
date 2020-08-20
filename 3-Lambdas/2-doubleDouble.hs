doubleDouble_v1 x =
    double * 2
    where double = x * 2

doubleDouble_v2 x =
    (\double -> double * 2) (x * 2)

doubleDouble_v3 x =
    let double = x * 2 in
        double * 2