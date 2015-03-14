triples = [(a, b, c) | a <- [1..998]
                     , b <- [1..(999-a)]
                     , let c = 1000 - (a + b)
                     , a*a + b*b == c*c
                     ]

answer = (\(a, b, c) -> a*b*c) $ head triples

main = print answer

