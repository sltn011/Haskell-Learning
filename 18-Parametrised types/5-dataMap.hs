import qualified Data.Map as Map

data Organ = Heart | Liver | Brain | Kidney deriving(Show, Eq)

organs :: [Organ]
organs = [Heart, Liver, Liver, Liver, Kidney, Brain, Brain]

ids :: [Int]
ids = [2, 7, 13, 15, 21, 30, 42]

-- Создаем словарь с помощью Map.fromList

numeredOrgans :: [(Int, Organ)]
numeredOrgans = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList numeredOrgans

-- поиск по ключу - Map.lookup k - Возвращает Maybe