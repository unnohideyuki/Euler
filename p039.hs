import Data.List

isTriangle   (a, b, c) = (a < b + c) && (b < c + a) && (c < a + b)
isRightAngle (a, b, c) = let a2 = a*a
                             b2 = b*b
                             c2 = c*c
                         in (a2 == b2 + c2) || (b2 == c2 + a2) || (c2 == a2 + b2)
                           
-- right angle triangles                            
rats p = nub [(x, y, z) | a <- [1..(p-2)], b <- [1..(p-a)], let c = p - a - b
                        , isTriangle (a, b, c)
                        , isRightAngle (a, b, c)
                        , let (x:y:z:[]) = sort [a, b, c]]

findAnswer mp _  []     = mp
findAnswer mp mn (p:ps) = let n   = length $ rats p
                              mn' = max mn n
                              mp' = if mn > n then mp else p
                          in findAnswer mp' mn' ps

answer = findAnswer 0 0 [3..1000]

main = print answer