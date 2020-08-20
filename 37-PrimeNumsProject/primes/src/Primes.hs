module Primes where

maxNum :: Int
maxNum = 100000

primes :: [Int]
primes = sieve [2 .. maxNum]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime : rest) =
    nextPrime : sieve noFactors
    where noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | n >= maxNum = Nothing
          | otherwise = Just (n `elem` primes)

unsafePrimeFactor :: Int -> [Int] -> [Int]
unsafePrimeFactor 0 [] = []
unsafePrimeFactor n [] = []
unsafePrimeFactor n (next : primes) =
    if n `mod` next == 0
        then next : unsafePrimeFactor (n `div` next) (next : primes)
        else unsafePrimeFactor n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
              | n >= maxNum = Nothing
              | otherwise = Just (unsafePrimeFactor n primesLessThanN)
              where primesLessThanN = filter (<= n) primes
