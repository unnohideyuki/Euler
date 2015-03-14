module ForPrjEuler where

import Data.List
import qualified Data.Set as Set

isqrt :: Integral a => a -> a
isqrt = truncate.sqrt.fromIntegral

primeMax :: Int
primeMax  = 1000000

primes :: [Int]
primes  = 2 : sieve [3, 5..primeMax]
  where
    n = isqrt primeMax
    sieve [] = []
    sieve (p : xs)
      | p > n     = p : xs
      | otherwise = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime  :: Int -> Bool
isPrime x = Set.member x primeSet
  where primeSet = Set.fromList primes

divisors  :: Int -> [Int]
divisors x = f x m []
  where m = isqrt x
        f n d ds | d == 0    = ds
                 | otherwise = let ds' = if n `mod` d == 0
                                         then ds ++ nub [d, n `div` d]
                                         else ds
                               in f x (d-1) ds'

