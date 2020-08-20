import System.Environment

getOperation :: (Num a, Fractional a) => String -> (a -> a -> a)
getOperation symb =
    case symb of
        "+" -> (+)
        "-" -> (-)
        "*" -> (*)
        "/" -> (/)
        _ -> error "Incorrect input! Inavailable operation"

parseInput :: String -> [String]
parseInput usrInput = lines usrInput

toDouble :: String -> Double
toDouble str = read str

main :: IO ()
main = do
    args <- getArgs
    if length args == 3
        then do
            let a = toDouble ((!!) args 0)
            let f = getOperation ((!!) args 1)
            let b = toDouble ((!!) args 2)
            print (f a b)
        else do
            error "Incorrect input"