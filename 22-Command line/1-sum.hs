import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args == 1
                          then read (head args)
                          else 0 :: Int
    nums <- replicateM linesToRead getLine
    let ints = map read nums :: [Int]
    print (sum ints)