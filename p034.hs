import Data.Array

fac n = a ! n
  where a = array (0, 9) $ [(0, 1)] ++ [(i, fac' i) | i <- [1..9]]
        fac' n = product [1..n]
        
f xs = sum $ map (\x -> fac x) xs

g xs = pr xs ys
  where
    ys = [10^i | i <- [0..6]]
    pr [] _  = 0
    pr _  [] = 0
    pr (a:as) (b:bs) = a*b + pr as bs
    

cands = filter (\xs -> f xs == g xs) ns
  where
    ns = ns2 ++ ns3 ++ ns4 ++ ns5 ++ ns6 ++ ns7
    ns2 = [[a, b] | a <- [0..9], b <- [1..9]]
    ns3 = [[a, b, c]             | a <- [0..9]
                                 , b <- [0..9]
                                 , c <- [1..9]]
    ns4 = [[a, b, c, d]          | a <- [0..9]
                                 , b <- [0..9]
                                 , c <- [0..9]
                                 , d <- [1..9]]
    ns5 = [[a, b, c, d, e]       | a <- [0..9]
                                 , b <- [0..9]
                                 , c <- [0..9]
                                 , d <- [0..9]
                                 , e <- [1..9]]
    ns6 = [[a, b, c, d, e, f]    | a <- [0..9]
                                 , b <- [0..9]
                                 , c <- [0..9]
                                 , d <- [0..9]
                                 , e <- [0..9]
                                 , f <- [1..9]]
    ns7 = [[a, b, c, d, e, f, g] | a <- [0..9]
                                 , b <- [0..9]
                                 , c <- [0..9]
                                 , d <- [0..9]
                                 , e <- [0..9]
                                 , f <- [0..9]
                                 , g <- [1..4]]

answer = sum $ map g cands

main = print answer


        
                                             