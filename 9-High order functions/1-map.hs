myMap _ [] = []

myMap func (x : xs) =
    (func x) : myMap func xs