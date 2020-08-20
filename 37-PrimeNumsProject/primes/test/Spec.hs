import Test.QuickCheck
import Primes
import Data.Maybe

prop_validPrimesOnly val =
    if val < 2 || val >= maxNum
        then result == Nothing
        else isJust result
    where result = isPrime val

prop_primesArePrimes val = if result == Just True
    then length divisors == 0
    else True
    where result = isPrime val
          divisors = filter ((== 0) . (val `mod`)) [2 .. (val `div` 2)]

prop_nonPrimesAreComplex val = if result == Just False
    then length divisors /= 0
    else True
    where result = isPrime val
          divisors = filter ((== 0) . (val `mod`)) [2 .. (val `div` 2)]

prop_factorsMakeOriginal val =
    if maybeResult == Nothing
        then True
        else product result == val
    where maybeResult = primeFactors val
          result = fromJust maybeResult

prop_allFactorsArePrimes val =
    if result == Nothing
        then True
        else all (== Just True) factors
    where result = primeFactors val
          factors = map isPrime (fromJust result)

main :: IO ()
main = do
    quickCheckWith stdArgs {maxSuccess = 10000} prop_validPrimesOnly
    quickCheckWith stdArgs {maxSuccess = 10000} prop_primesArePrimes
    quickCheckWith stdArgs {maxSuccess = 10000} prop_nonPrimesAreComplex
    quickCheckWith stdArgs {maxSuccess = 10000} prop_factorsMakeOriginal
    quickCheckWith stdArgs {maxSuccess = 10000} prop_allFactorsArePrimes
