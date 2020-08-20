{-
 - fileToTS :: [(Int, a)] -> TS a
 - meanTS - среднее значение по временному ряду
 - minTS maxTS - наибольшее и наименьшее значение во временном ряду
 - diffTS - временной ряд изменения значения за единицу времени
 - movingAverageTS - сглаживание данных используя скользящее среднее
 - medianTS - медиана по временному ряду
 - movingMedianTS - сглаживание данных используя скользящую медиану
 - percDiffTS - временной ряд изменения значения за единицу времени в процентах
 - stdDeviationTS - среднеквадратиное отклонение значений во временном ряду (несмещенная оценка дисперсии)
 - combineTSWith - объединение временных рядов с использованием функции типа CombineTSVals a = Maybe a -> Maybe a -> Maybe a
 -}

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int, Double)]
file1 = [(1, 200.1), (2, 199.5), (3, 199.4), (4, 198.9)
       , (5, 199.0), (6, 200.2), (9, 200.3), (10, 201.2)
       , (12, 202.9)]

file2 :: [(Int, Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5), (14, 203.5)
       , (15, 204.9), (16, 207.1), (18, 210.5), (20, 208.8)]

file3 :: [(Int, Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5), (13, 201.5)
       , (14, 203.5), (17, 210.5), (24, 215.1), (25, 218.7)]

file4 :: [(Int, Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8), (29, 222.8)
       , (30, 223.8), (31, 221.7), (32, 222.3), (33, 220.8)
       , (34, 219.4), (35, 220.1), (36, 220.6)]

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
    where completeTimes = [minimum times .. maximum times]
          timeValueMap = Map.fromList (zip times values)
          extendedValues = map (\t -> Map.lookup t timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS file = createTS times values
    where (times, values) = unzip file

showTVPair :: (Show a) => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, " | ", show value, "\n"]
showTVPair time Nothing = mconcat [show time, " | NA\n"]

instance (Show a) => Show (TS a) where
    show (TS times values) = mconcat rows
        where rows = zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

insertMaybePair :: (Ord k) => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, Just val) = Map.insert key val myMap  

combineTS :: TS a -> TS a -> TS a
combineTS ts1 (TS [] []) = ts1
combineTS (TS [] []) ts2 = ts2
combineTS (TS t1 v1) (TS t2 v2) = TS combinedTime combinedValues
    where bothTimes = mconcat [t1, t2]
          combinedTime = [minimum bothTimes .. maximum bothTimes]
          tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
          updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
          combinedValues = map (\v -> Map.lookup v updatedMap) combinedTime

instance Semigroup (TS a) where
    (<>) ts1 ts2 = combineTS ts1 ts2

instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double
mean xs = total / count
    where total = (realToFrac . sum) xs
          count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) =
    if all (== Nothing) values
        then Nothing
        else Just avg
    where justValues = filter isJust values
          onlyValues = map fromJust justValues
          avg = mean onlyValues

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: (Eq a) => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
    where
        newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
        newFunc (_, Nothing) (i, val) = (i, val)
        newFunc (i, val) (_, Nothing) = (i, val)
        newFunc (i1, Just val1) (i2, Just val2) =
            if func val1 val2 == val1
                then (i1, Just val1)
                else (i2, Just val2)

compareTS :: (Eq a) => CompareFunc a -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) =
    if all (== Nothing) values
        then Nothing
        else Just best
    where
        pairs = zip times values
        best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: (Ord a) => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: (Ord a) => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

diffPair :: (Num a) => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: (Num a) => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing : diffValues)
    where shiftValues = tail values
          diffValues = zipWith diffPair shiftValues values

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals =
    if any (== Nothing) vals
        then Nothing
        else (Just avg)
    where avg = mean (map fromJust vals)

movingAverage :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAverage [] n = []
movingAverage vals n =
    if length nextVals == n
        then meanMaybe nextVals : movingAverage restVals n
        else []
    where nextVals = take n vals
          restVals = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) _ = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
    where ma = movingAverage values n
          nothings = replicate (n `div` 2) Nothing
          smoothedValues = mconcat [nothings, ma, nothings]

median :: (Real a) => [a] -> Double
median vals = medianVal
    where sortedVals = sortBy (\x y -> compare y x) vals
          valsLen = length vals
          medianVal = if odd valsLen
                          then realToFrac (sortedVals !! middle)
                          else (mean.(take 2).reverse.(take (middle + 1))) sortedVals
                      where middle = valsLen `div` 2

medianMaybe :: (Real a) => [Maybe a] -> Maybe Double
medianMaybe [] = Nothing
medianMaybe vals =
    if any (== Nothing) vals
        then Nothing
        else Just medianVal
    where onlyJust = filter isJust vals
          onlyValues = map fromJust onlyJust
          medianVal = median onlyValues       

medianTS :: (Real a) => TS a -> Maybe Double
medianTS (TS _ []) = Nothing
medianTS (TS times values) =
    if all (== Nothing) values
        then Nothing
        else Just medianVal
    where onlyJust = filter isJust values
          onlyValues = map fromJust onlyJust
          medianVal = median onlyValues
        
movingMedian :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingMedian [] _ = []
movingMedian values n =
    if length nextVals == n
        then medianMaybe nextVals : movingMedian restVals n
        else []
    where nextVals = take n values
          restVals = tail values

movingMedianTS :: (Real a) => TS a -> Int -> TS Double
movingMedianTS (TS [] []) _ = TS [] []
movingMedianTS (TS times values) n = TS times smoothedValues
    where mm = movingMedian values n
          nothings = replicate (n `div` 2) Nothing
          smoothedValues = mconcat [nothings, mm, nothings]

percDiffPair :: (Fractional a) => Maybe a -> Maybe a -> Maybe a
percDiffPair Nothing _ = Nothing
percDiffPair _ Nothing = Nothing
percDiffPair (Just x) (Just y) = Just (x / y)

percDiffTS :: (Fractional a) => TS a -> TS a
percDiffTS (TS _ []) = TS [] []
percDiffTS (TS times values) = TS times (Nothing : diffValues)
    where shiftValues = tail values
          diffValues = zipWith percDiffPair shiftValues values

stdDeviation :: (Real a) => [a] -> Double
stdDeviation values = sqrt disp
    where meanVal = mean values
          updatedVals = map realToFrac values
          differences = zipWith (-) updatedVals (repeat meanVal)
          diffSqrs = map (\x -> x ^ 2) differences
          sumSqrs = sum diffSqrs
          numVals = length values
          disp = sumSqrs / (fromIntegral (numVals - 1))

stdDeviationMaybe :: (Real a) => [Maybe a] -> Maybe Double
stdDeviationMaybe [] = Nothing
stdDeviationMaybe values =
    if all (== Nothing) values
        then Nothing
        else Just deviation
    where onlyValues = ((map fromJust).(filter isJust)) values
          deviation = stdDeviation onlyValues

stdDeviationTS :: (Real a) => TS a -> Maybe Double
stdDeviationTS (TS [] []) = Nothing
stdDeviationTS (TS times values) = stdDeviationMaybe values

type CombineTSVals a = Maybe a -> Maybe a -> Maybe a

combineTSWith :: CombineTSVals a -> TS a -> TS a -> TS a
combineTSWith _ (TS [] []) ts = ts
combineTSWith _ ts (TS [] []) = ts
combineTSWith func (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where times = t1 <> t2
          completeTimes = [minimum times .. maximum times]
          timeVal1 = zip t1 v1
          timeVal2 = zip t2 v2
          onlyJust1 = filter (\(_, v) -> isJust v) timeVal1
          onlyJust2 = filter (\(_, v) -> isJust v) timeVal2
          onlyValues1 = map (\(t, v) -> (t, fromJust v)) onlyJust1
          onlyValues2 = map (\(t, v) -> (t, fromJust v)) onlyJust2
          mapV1 = Map.fromList onlyValues1
          mapV2 = Map.fromList onlyValues2
          filledV1 = map ((flip Map.lookup) mapV1) completeTimes
          filledV2 = map ((flip Map.lookup) mapV2) completeTimes
          combinedValues = zipWith func filledV1 filledV2

makeTSValsCombiner :: (a -> a -> a) -> CombineTSVals a
makeTSValsCombiner func = newFunc
    where newFunc Nothing v = v
          newFunc v Nothing = v
          newFunc (Just v1) (Just v2) = Just (func v1 v2)