data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize letter =
    toEnum encrypted
    where letterNum = fromEnum letter
          halfOfAlphabet = alphabetSize `div` 2
          offset = letterNum + halfOfAlphabet
          encrypted = offset `mod` alphabetSize

rotNBack :: (Bounded a, Enum a) => Int -> a -> a
rotNBack alphabetSize letter =
    toEnum decrypted
    where letterNum = fromEnum letter
          halfOfAlphabet = alphabetSize `div` 2
          offset = letterNum + halfOfAlphabet + (alphabetSize `mod` 2)
          decrypted = offset `mod` alphabetSize

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder message = encrypted
    where alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          encrypted = map (rotN alphabetSize) message

fourLetterDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterDecoder message = encrypted
    where alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          encrypted = map (rotNBack alphabetSize) message