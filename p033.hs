import Data.List

pairs :: [(Int, Int)]
pairs = [(n, d) | d <- [11..99], n <- [10..(d-1)]]

simplify :: (Int, Int) -> [(Int, Int)]
simplify (n, d) = let sn = show n
                      sd = show d
                  in [(n', d') | c <- sn
                               , c /= '0'
                               , elem c sd
                               , let n' = read $ sn \\ [c]
                                     d' = read $ sd \\ [c]]

pairEqual (n, d) (n', d') = n*d' == n'*d

isNonTrivialExample (n, d) = or $ map (pairEqual (n, d)) (simplify (n, d))

productOfExamples = foldr (\(n1, d1) (n2, d2) -> (n1*n2, d1*d2))
                          (1, 1)
                          (filter isNonTrivialExample pairs)
                    
answer = let (n, d) = productOfExamples
         in d `div` gcd n d
            
main = print answer

