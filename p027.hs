import ForPrjEuler

checkQuadratic a b = check [0..]
  where f n = n*n + a*n + b
        check (x:xs) = if isPrime (f x) then check xs else x

findBest pair _    []     = pair
findBest pair best ((a,b):xs) = let l = checkQuadratic a b
                                in if best > l
                                   then findBest pair  best xs
                                   else findBest (a,b) l    xs

bestPair = findBest (0, 0) 0 [(a, b) | a <- [-999..999], b <- [-999..999]]

answer = case bestPair of (a, b) -> a * b

main = print answer


