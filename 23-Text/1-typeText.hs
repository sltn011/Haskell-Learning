{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

-- T.pack :: String -> T.Text
-- T.unpack :: T.Text -> String

text1 :: T.Text
text1 = "This\nis\ntext"

text2 :: T.Text
text2 = "This is another text"
-- Просто так нельзя (и это тупо что нельзя)
-- поэтому необходимо подключить расширение языка с помощью
-- {-# LANGUAGE OverloadedStrings #-}

text1Lines :: [T.Text]
text1Lines = T.lines text1

text2Words :: [T.Text]
text2Words = T.words text2

breakText :: T.Text
breakText = "just"

text3 :: T.Text
text3 = "This is just another text"

text3Broken :: [T.Text]
text3Broken = T.splitOn breakText text3

backToText1 :: T.Text
backToText1 = T.unlines text1Lines

backToText2 :: T.Text
backToText2 = T.unwords text2Words

backToText3 :: T.Text
backToText3 = T.intercalate breakText text3Broken