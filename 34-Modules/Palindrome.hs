module Palindrome (isPalindrome) where

import Data.Char (isSpace, isPunctuation, toLower)

removeSpaces :: String -> String
removeSpaces str = filter (not . isSpace) str

removePunctuation :: String -> String
removePunctuation str = filter (not . isPunctuation) str

toLowerCase :: String -> String
toLowerCase str = map toLower str

process :: String -> String
process str = (toLowerCase . removePunctuation . removeSpaces) str

isPalindrome :: String -> Bool
isPalindrome str = (cleanStr == reverse cleanStr)
    where cleanStr = process str