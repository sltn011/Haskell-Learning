--Functor нужен для использование типов в одном контексте в функции без контекста

--нужно instance Functor Datatype where fmap func ... = ...

--fmap func Datatype - можно работать с функциями без контекста
--или func <$> Datatype

val1 :: Maybe Int
val1 = Just 5

val2 :: Maybe Int
val2 = Nothing

foo :: Maybe Int -> Maybe String
foo val = (show . succ) <$> val