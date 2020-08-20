data MyEnumType = E1 | E2 | E3 deriving(Enum)

instance Eq MyEnumType where
    (==) l r = (fromEnum l) == (fromEnum r)

instance Ord MyEnumType where
    compare l r = compare (fromEnum l) (fromEnum r)


    
class (Enum a, Eq a, Ord a) => Die a where
    getSide :: a -> String

data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Ord)

instance Die FiveSidedDie where
    getSide S1 = "One"
    getSide S2 = "Two"
    getSide S3 = "Three"
    getSide S4 = "Four"
    getSide S5 = "Five"