--class Show a where
--    show :: a -> String

data Icecream = Vanilla | Chocolate deriving(Show, Eq)
--если тут добавит еще и Ord или Enum, то Vanilla будет считаться меньшим(написана раньше в определении :ь)

--Теперь можно вывести тип мороженного и проверять их на идентичность

doubleSucc :: (Enum a) => a -> a
doubleSucc n = succ (succ n)

cycleSucc :: (Enum a, Bounded a, Eq a) => a -> a
cycleSucc n =
    if max == n
        then least
        else succ n
    where max = maxBound
          least = minBound