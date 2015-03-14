import Data.List
import Data.Maybe
import qualified Data.Set as Set

primeMax = 2000000

primes = 2 : sieve [3, 5..primeMax]
  where
    n = (truncate.sqrt.fromIntegral) primeMax
    sieve (p : xs)
      | p > n     = p : xs
      | otherwise = p : sieve [x | x <- xs, x `mod` p /= 0]
  
sum' acc [] = acc
sum' acc (p:ps) = sum' (acc + p) ps


answer = sum' 0 primes
                                         
main = print answer

  