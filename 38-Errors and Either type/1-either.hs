{-
Ф-ция head плоха - она кидает исключение на пустом списке

head :: [a] -> a
head [] = error!!!! - ПЛОХО!!!! НЕПРЕДСКАЗУЕМОЕ ПОВЕДЕНИЕ ПРОГРАММЫ
head (x : _) = x

Улучшить себе жизнь поможет тип Either
Either a b = Left a | Right b
--}

data ListError = EmptyList

instance Show ListError where
    show EmptyList = "Переданный в функцию список оказался пустым!"

myHead :: [a] -> Either ListError a
myHead [] = Left EmptyList
myHead (x : _) = Right x