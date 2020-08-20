module Palindrome (isPalindrome) where

import Data.Char (isSpace, isPunctuation, toLower)
import qualified Data.Text as T

removeSpaces :: T.Text -> T.Text
removeSpaces str = T.filter (not . isSpace) str

removePunctuation :: T.Text -> T.Text
removePunctuation str = T.filter (not . isPunctuation) str

toLowerCase :: T.Text -> T.Text
toLowerCase str = T.toLower str

process :: T.Text -> T.Text
process str = (toLowerCase . removePunctuation . removeSpaces) str

isPalindrome :: T.Text -> Bool
isPalindrome str = (cleanStr == T.reverse cleanStr)
    where cleanStr = process str