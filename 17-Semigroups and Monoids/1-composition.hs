import Data.List

myLast :: [a] -> a
myLast = head.reverse

myMin :: Ord a => [a] -> a
myMin = head.sort

myMax :: Ord a => [a] -> a
myMax = head.reverse.sort -- myLast.sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll boolFunc = (foldl (&&) True).(map boolFunc)