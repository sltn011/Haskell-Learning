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

{-
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
-}

main :: IO ()
main =
    putStrLn "Input first pizza size" >>
    getLine >>=
        (\p1Size -> putStrLn "Input first pizza cost" >>
        getLine >>=
            (\p1Cost -> putStrLn "Input second pizza size" >>
            getLine >>=
                (\p2Size -> putStrLn "Input second pizza cost" >>
                getLine >>=
                    (\p2Cost ->
                        (\p1 -> 
                            (\p2 ->
                                (\pBest ->
                                    putStrLn (describePizza pBest))
                                    (comparePizzas p1 p2))
                                    (read p2Size, read p2Cost))
                                    (read p1Size, read p1Cost)))))

{-
maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizesCatalog
    price1 <- Map.lookup 1 pricesCatalog
    size2 <- Map.lookup 2 sizesCatalog
    price2 <- Map.lookup 2 pricesCatalog
    let pizza1 = (size1, price1) :: Pizza
    let pizza2 = (size2, price2) :: Pizza
    let bestPizza = comparePizzas pizza1 pizza2 :: Pizza
    return (describePizza bestPizza)
-}

listMain :: [String]
listMain = do
    size1 <- [12.0, 15.0, 20.0]
    price1 <- [10.0, 20.0, 30.0]
    size2 <- [15.0, 20.0, 25.0]
    price2 <- [14.5, 18.5, 22.0]
    let pizza1 = (size1, price1) :: Pizza
    let pizza2 = (size2, price2) :: Pizza
    let bestPizza = comparePizzas pizza1 pizza2 :: Pizza
    return (describePizza bestPizza)

allMMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
allMMain s1 p1 s2 p2 = do
    size1 <- s1
    price1 <- p1
    size2 <- s2
    price2 <- p2
    let pizza1 = (size1, price1) :: Pizza
    let pizza2 = (size2, price2) :: Pizza
    let bestPizza = comparePizzas pizza1 pizza2 :: Pizza
    return (describePizza bestPizza)