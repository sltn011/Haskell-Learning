data Circle = Circle { radius :: Double } deriving Show
data Square = Square { side :: Double } deriving Show
data Rectangle = Rectangle { sideA :: Double, sideB :: Double } deriving Show

data Shape = CircleShape Circle | SquareShape Square | RectangleShape Rectangle deriving Show

perimeter :: Shape -> Double
perimeter (CircleShape circle) = (2 * (radius circle) * pi)
perimeter (SquareShape square) = (4 * (side square))
perimeter (RectangleShape rect) = (2 * (sideA rect) + 2 * (sideB rect))

area :: Shape -> Double
area (CircleShape circle) = (pi * (radius circle)^2)
area (SquareShape square) = (side square)^2
area (RectangleShape rect) = (sideA rect) * (sideB rect)