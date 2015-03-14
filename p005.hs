f m i | i == 1    = m
      | otherwise = f (m * i `div` gcd m i) (i - 1)
                    
main = print $ f 2 20
