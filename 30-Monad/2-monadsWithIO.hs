echo :: IO ()
echo = getLine >>= putStrLn

echoVerbose :: IO ()
echoVerbose = putStrLn "Input something:" >> getLine >>= putStrLn

greeter :: IO ()
greeter = putStrLn "What's your name?" >> getLine >>= (\str -> putStrLn ("Hello, " ++ str ++ "!"))