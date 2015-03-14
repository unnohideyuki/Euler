n = 600851475143

primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
        
largest_prime_factor x = g x
  where g x = let x' = factor x primes
              in if x' == x then x else  g x'
        factor x (p:ps) | p >= x    = x
                        | otherwise = if x `mod` p == 0 
                                      then x `div` p
                                      else factor x ps
