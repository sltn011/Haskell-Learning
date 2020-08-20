data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

instance Show SixSidedDie where
    show S1 = "One"
    show S2 = "Two"
    show S3 = "Three"
    show S4 = "Four"
    show S5 = "Five"
    show S6 = "Six"

instance Eq SixSidedDie where
    (==) S1 S1 = True
    (==) S2 S2 = True
    (==) S3 S3 = True
    (==) S4 S4 = True
    (==) S5 S5 = True
    (==) S6 S6 = True
    (==) _ _ = False

instance Ord SixSidedDie where
    compare S6 S6 = EQ
    compare S6 _ = GT
    compare _ S6 = LT
    compare S5 S5 = EQ
    compare S5 _ = GT
    compare _ S5 = LT
    compare S4 S4 = EQ
    compare S4 _ = GT
    compare _ S4 = LT
    compare S3 S3 = EQ
    compare S3 _ = GT
    compare _ S3 = LT
    compare S2 S2 = EQ
    compare S2 _ = GT
    compare _ S2 = LT
    compare S1 S1 = EQ

instance Enum SixSidedDie where
    toEnum 0 = S1
    toEnum 1 = S2
    toEnum 2 = S3
    toEnum 3 = S4
    toEnum 4 = S5
    toEnum 5 = S6
    toEnum _ = error "Invalid argument"

    fromEnum S1 = 0
    fromEnum S2 = 1
    fromEnum S3 = 2
    fromEnum S4 = 3
    fromEnum S5 = 4
    fromEnum S6 = 5
-- Теперь можно сделать списки [S1 .. S6]
-- Бесконечный список [S1 ..] кидает ошибку после выхода за границу
-- Но ошибки не будет если делать deriving(Enum)

--Так что если можно - проще и лучше использовать порождение