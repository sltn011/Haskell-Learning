--module Main where можно не указывать явно, создается сам

import Palindrome

main :: IO ()
main = do
    putStrLn "Введите строку"
    input <- getLine
    let output = if isPalindrome input
        then "Ваша строка палиндром"
        else "Ваша строка не палиндром"
    putStrLn output