import Data.Numbers.Primes

checkGoldbach n = isTwiceASquare n 1
  where isTwiceASquare x y | y >= x                = False
                           | isPrime (x - 2*(y^2)) = True
                           | otherwise             = isTwiceASquare x (y+1)
                                                  
findFalse n | isPrime n || checkGoldbach n = findFalse (n+2)
            | otherwise = n
                          
answer = findFalse 3

main = print answer