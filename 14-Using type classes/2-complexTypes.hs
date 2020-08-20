import Data.List

data Name = Name (String, String) deriving (Show, Eq)

names :: [Name]
names = [ Name ("AAA", "ABC")
        , Name ("BCA", "AAA")
        , Name ("BBB", "ABB")
        , Name ("BAC", "AAA")]

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)