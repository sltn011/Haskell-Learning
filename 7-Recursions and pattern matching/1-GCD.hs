--GCD - Greatest Common Divisor
findGCD a b =
    if remainder == 0
        then b
        else findGCD b remainder
    where remainder = a `mod` b