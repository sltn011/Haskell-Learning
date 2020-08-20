{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello, ", name, "!"]

main :: IO ()
main = do
    TIO.putStrLn "What's your name?"
    name <- TIO.getLine
    let msg = helloPerson name
    TIO.putStrLn msg