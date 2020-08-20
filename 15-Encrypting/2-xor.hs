xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 =
    (value1 || value2) && (not(value1 && value2))

xorBoolsPair :: (Bool, Bool) -> Bool
xorBoolsPair (value1, value2) = xorBool value1 value2

type Bits = [Bool]

xorBits :: Bits -> Bits -> Bits
xorBits data1 data2 =
    map xorBoolsPair (zip data1 data2)

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
    intToBits' leftPart ++ [bit]
        where leftPart = n `div` 2
              bit = ((n `mod` 2) == 1)

intToBits :: Int -> Bits
intToBits n =
    missingBits ++ intInBits
        where intInBits = intToBits' n
              bitsLength = length intInBits
              bitsMissing = (length (intToBits' (maxBound :: Int)) - bitsLength)
              missingBits = take bitsMissing (cycle [False])
              
bitsToBinNum :: Bits -> [Int]
bitsToBinNum bits = map fromEnum bits

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits =
    sum (map (\x -> 2^(snd x)) meaningfulBits)
        where size = length bits
              powers = [size - 1, size - 2 .. 0]
              meaningfulBits = filter (\x -> (fst x) == True) (zip bits powers)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


xorStrings :: String -> String -> String
xorStrings string key =
    map bitsToChar encryptedBits
        where bitsString = map charToBits string
              bitsKey = map charToBits key
              encryptedBits = map (\pair -> (fst pair) `xorBits` (snd pair)) (zip bitsString (cycle bitsKey))