import ForPrjEuler

truncs :: Int -> [Int]
truncs x = let s = show x
               l = length s
               ys = [take i s | i <- [1..(l-1)]]
               zs = [drop i s | i <- [1..(l-1)]]
           in map read (ys++zs)

isBothTruncatable x = and $ map isPrime $ truncs x

bothTruncatablePrimes = take 11 $ filter isBothTruncatable $ drop 4 primes

answer = sum bothTruncatablePrimes

main = print answer



