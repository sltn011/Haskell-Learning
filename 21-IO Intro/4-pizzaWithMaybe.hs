import qualified Data.Map as Map

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

sizesCatalog :: Map.Map Int Double
sizesCatalog = Map.fromList [(1, 30), (2, 25)]

pricesCatalog :: Map.Map Int Double
pricesCatalog = Map.fromList [(1, 500), (2, 430)]

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