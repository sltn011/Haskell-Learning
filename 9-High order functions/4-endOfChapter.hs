import Data.Char

myElem n xs =
    length (filter ((==) n) xs) > 0

isPalindrome string =
    word == reverse word
        where word = map toUpper (filter isAlpha string)

harmonic n =
    foldr (+) 0 series
    where list = [1 .. n]
          series = map (flip (**) (-1)) list