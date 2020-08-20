--type (+)
--Num a => a -> a -> a ????? KAVO? WHO?

--info Num показывает методы класса типов Num

class Describable a where
    describe :: a -> String

--type (>) даёт Ord
--info Ord: class Eq a => Ord a where...