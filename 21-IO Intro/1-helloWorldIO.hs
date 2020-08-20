helloPerson :: String -> String
helloPerson name = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "What's your name?"
    name <- getLine
    let msg = helloPerson name
    putStrLn msg