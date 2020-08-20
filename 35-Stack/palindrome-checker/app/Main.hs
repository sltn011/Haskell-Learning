module Main where

import Palindrome
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "Введите строку"
    input <- TIO.getLine
    let output = if isPalindrome input
        then "Ваша строка палиндром"
        else "Ваша строка не палиндром"
    TIO.putStrLn output
