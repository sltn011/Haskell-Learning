foo x y =
    x ^ y

flipBinaryArgs binaryFunc = -- то же что и flip
    (\y x -> binaryFunc x y)

-- инфиксную операцию (например '+' можно сделать префиксной: (+) х у)

--функция вычитающая из числа двойку:
decreaseBy2 = flip (-) 2 --(\x -> (-) x 2)