firstNPrimes :: Int -> [Int]
firstNPrimes n = filter isNotPrime elems
    where elems = [2 .. n]
          allProducts = pure (*) <*> elems <*> elems
          isNotPrime = not . (`elem` allProducts)