-- simpleInt :: Int -> Int
-- simpleDouble :: Double -> Double
-- simpleString :: String -> String - НЕ УДОБНО

simple :: a -> a
simple x = x

makeTrio :: a -> b -> c -> (a, b, c)
makeTrio x y z = (x, y, z)