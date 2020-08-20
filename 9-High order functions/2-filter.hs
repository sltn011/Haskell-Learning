myFilter _ [] = []

myFilter func (x : xs) =
    if match
        then x : myFilter func xs
        else myFilter func xs
    where match = func x

remove _ [] = []

remove func (x : xs) =
    if match
        then remove func xs
        else x : remove func xs
    where match = func x