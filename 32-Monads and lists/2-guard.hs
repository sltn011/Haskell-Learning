import Control.Monad

evensGuard :: Int -> [Int]
evensGuard n = do
    val <- [1 .. n]
    guard (even val) -- == filter
    return val