toInts :: String -> [Int]
toInts numsStr = result
    where parsedLines = lines numsStr
          result = map read parsedLines :: [Int]


main :: IO ()
main = do
    userInput <- getContents
    let values = toInts userInput
    putStrLn (show (sum values))