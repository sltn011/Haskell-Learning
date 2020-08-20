import qualified Data.Map as Map
import Data.List

data Organ = Heart | Liver | Brain | Kidney deriving (Eq)

instance Show Organ where
    show Heart = "Heart"
    show Liver = "Liver"
    show Brain = "Brain"
    show Kidney = "Kidney"

organs :: [Organ]
organs = [Heart, Heart, Liver, Liver, Liver, Brain, Brain, Kidney]

ids :: [Int]
ids = [2, 5, 7, 12, 16, 21, 27, 30]

numeredOrgans :: [(Int, Organ)]
numeredOrgans = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList numeredOrgans

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawersContent :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawersContent possibleDrawers catalog = map getContent possibleDrawers
    where getContent = (\id -> Map.lookup id catalog)

drawersContent :: [Maybe Organ]
drawersContent = getDrawersContent possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ organsList = length (filter (\x -> x == Just organ) organsList)

hasSomething :: Maybe Organ -> Bool
hasSomething (Just _) = True
hasSomething Nothing = False

justOrgans :: [Maybe Organ]
justOrgans = filter hasSomething drawersContent

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

cleanList :: String
cleanList = intercalate ", " (map showOrgan justOrgans)




data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ) = mconcat [show organ, " in vat"]
    show (Cooler organ) = mconcat [show organ, " in cooler"]
    show (Bag organ) = mconcat [show organ, " in bag"]

data Location = Lab | Kitchen | Bathroom

instance Show Location where
    show Lab = "In lab"
    show Kitchen = "In kitchen"
    show Bathroom = "In bathroom"

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen , Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation containerWithOrgan
    where containerWithOrgan = organToContainer organ

report :: (Location, Container) -> String
report (loc, cont) = mconcat [show cont, " (place: ", show loc, " )"]

processAndReport :: (Maybe Organ) -> String
processAndReport Nothing = "No organ"
processAndReport (Just organ) = (report.process) organ

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id boxes = processAndReport box
    where box = Map.lookup id boxes