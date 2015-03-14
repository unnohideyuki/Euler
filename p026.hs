recLength     :: [Int] -> Int -> Int
recLength xs d = let n = head xs
                     y = (n `mod` d) * 10
                 in if y == 0 then 0 else
                      case lookup y $ zip xs [1..] of
                        Just i  -> i
                        Nothing -> recLength (y:xs) d

findMax (d, _) []     = d
findMax (d, m) (x:xs) = let m' = recLength [10] x
                        in if m > m'
                           then findMax (d, m) xs
                           else findMax (x, m') xs

answer = findMax (0, 0) [1..999]

main = print answer

