--отличие Int и Integer
-- info int показывает дополнительную строку Bounded Int

class Bounded a where
    minBound :: a
    maxBound :: a

--minBound :: Int
--maxBound :: Int
--minBound :: Char
--maxBound :: Char