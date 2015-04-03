
-- ways :: How many different ways to take some number x from n, 
--         that satisfies x <= n and x <=m.
ways 0 _ = 1
ways n m = let n' = min n m
           in  sum [ways (n-a) a | a <- [1..n']]
               
answer = ways 100 99

main = print answer

