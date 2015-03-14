import ForPrjEuler

import qualified Data.Set as Set

isAbundant x = Set.member x s
  where s = Set.fromList abundants

abundants = [x | x <- [12..28123], isAbs x]
  where isAbs x = (sum $ divisors x) > 2 * x


isSumOfAbs x = f x abundants
  where f _ []     = False
        f x (a:as) | a >= x    = False
                   | otherwise = isAbundant (x-a) || f x as

answer = sum [x | x <- [1..28123], not $ isSumOfAbs x]

main = print answer


        

                

