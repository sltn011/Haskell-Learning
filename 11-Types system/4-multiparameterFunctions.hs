-- func name  1st arg, 2nd arg, 3rd arg    return type
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

--На самом деле все функции принимают лямбду
makeAddressLambda = 
    (\number ->
        (\street ->
            (\town -> (number, street, town))))

ifEvenDo :: (Int -> Int) -> Int -> Int
ifEvenDo func n =
    if even n then
        func n
        else n