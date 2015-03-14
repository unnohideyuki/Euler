import Data.List
import Data.Maybe
import qualified Data.Set as Set

primeMax = 1000000

primes = 2 : sieve [3, 5..primeMax]
  where
    n = (truncate.sqrt.fromIntegral) primeMax
    sieve (p : xs)
      | p > n     = p : xs
      | otherwise = p : sieve [x | x <- xs, x `mod` p /= 0]
  
isPrime x = Set.member x primeSet
  where primeSet = Set.fromList primes

rotPrimes ps = nub [rot x | x <- ps, isPrime (rot x)]
  where rot :: Int -> Int
        rot x  = read $ case show x of (c:cs) -> cs ++ [c]

answer = (length.f.f.f.f.f.f) primes where f = rotPrimes
                                         
main = print answer

  