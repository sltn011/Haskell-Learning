import Data.Semigroup

data Color = Red | Green | Blue | Yellow | Orange | Purple | Brown deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b
        | a == b = a
        | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
        | all (`elem` [Yellow, Blue, Green]) [a, b] = Green
        | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
        | otherwise = Brown
        
