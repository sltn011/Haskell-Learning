{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

dharma :: T.Text
dharma = "धर्म"

highlight :: T.Text -> T.Text -> T.Text
highlight key text = result
    where splitOnKey = T.splitOn key text
          highlightedKey = mconcat ["{", key, "}"]
          result = T.intercalate highlightedKey splitOnKey

main :: IO ()
main = do
    TIO.putStrLn (highlight dharma bgText)