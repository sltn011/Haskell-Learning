import qualified Data.Map as Map

type LatLong = (Double, Double)

main :: IO ()
main = do
    putStrLn "Input first city name:"
    cityName1 <- getLine
    putStrLn "Input second city name:"
    cityName2 <- getLine
    let city1 = Map.lookup cityName1 locDB
    let city2 = Map.lookup cityName2 locDB
    let dist = getDistance city1 city2
    printDistance dist

locDB :: Map.Map String LatLong
locDB = Map.fromList [ ("Ark", (42.6054, -70.7829))
                     , ("Insmout", (42.850, -70.8120))
                     , ("Karkoza", (29.9714, -90.7694))
                     , ("New York", (40.7776, -73.9691)) ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
    where rlat = toRadians lat
          rlong = toRadians long

harversine :: LatLong -> LatLong -> Double
harversine coords1 coords2 = earthRadius * c
    where (rlat1, rlong1) = latLongToRads coords1
          (rlat2, rlong2) = latLongToRads coords2
          dlat = rlat2 - rlat1
          dlong = rlong2 - rlong1
          a = (sin (dlat/2))^2 + (cos rlat1) * (cos rlat2) * (sin (dlong/2))^2
          c = 2 * atan2 (sqrt a) (sqrt (1-a))
          earthRadius = 6378.1

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "You inputted city that is not in DB!"
printDistance (Just dist) = putStrLn (show dist ++ " km")

getDistance :: Maybe LatLong -> Maybe LatLong -> Maybe Double
getDistance pointA pointB = harversine <$> pointA <*> pointB