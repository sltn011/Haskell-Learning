import System.Random

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

roll :: IO ()
roll = do
    val <- randomRIO(minDie, maxDie)
    putStrLn ("You rolled: " ++ show val)