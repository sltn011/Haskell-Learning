areaGivenDiameter :: Double -> Double
areaGivenDiameter diam = pi * ((diam / 2) ^ 2)

type Pizza = (Double, Double)

costPerCm :: Pizza -> Double
costPerCm (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                          then p1
                          else p2
                      where costP1 = costPerCm p1
                            costP2 = costPerCm p2

describePizza :: Pizza -> String
describePizza (size, cost) = mconcat 
    ["Pizza with size ", show size, " is cheaper and costs ",
    show (costPerCm (size, cost)), " per cm"]

main :: IO ()
main = do
    putStrLn "Input first pizza size"
    p1Size <- getLine
    putStrLn "Input first pizza cost"
    p1Cost <- getLine
    putStrLn "Input second pizza size"
    p2Size <- getLine
    putStrLn "Input second pizza cost"
    p2Cost <- getLine
    let p1 = (read p1Size, read p1Cost) :: Pizza
    let p2 = (read p2Size, read p2Cost) :: Pizza
    let pBest = comparePizzas p1 p2
    putStrLn (describePizza pBest)