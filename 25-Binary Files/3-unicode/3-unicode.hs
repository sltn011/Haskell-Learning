{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

shitStringBC :: BC.ByteString
shitStringBC = "श्रेयान्स्वधर्मो विगुणः"

shitStringText :: T.Text
shitStringText = "श्रेयान्स्वधर्मो विगुणः"

shitStringB :: B.ByteString -- == shitStringBC
shitStringB = (BC.pack . T.unpack) shitStringText

shitStringBReverted :: T.Text
shitStringBReverted = (T.pack . BC.unpack) shitStringB

shitStringSafe :: B.ByteString
shitStringSafe = E.encodeUtf8 shitStringText