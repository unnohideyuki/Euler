import ForPrjEuler

isAmicable x = let y = d x in x == d y && x /= y
  where d n = (sum $ divisors n) - n

answer = sum $ [x | x <- [2..10000], isAmicable x]

main = print answer
