double :: Int -> Int -- функция с именем double, принимающая Int и возвращающая Int
double n = n * 2

half :: Int -> Double
half n = (fromIntegral n) / 2

halve :: Integer -> Integer
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (n * 2)

number :: Int
number = read "6"
-- В GHCi можно использовать read "6" :: Int