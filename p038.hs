import Data.List

xs = [x | let d3 = ['8', '7'..'1']
        , a <- d3, let d2 = d3 \\ [a]
        , b <- d2, let d1 = d2 \\ [b]
        , c <- d1
        , let x = ['9', a, b, c]]
     
isOk s = let n :: Int
             n  = read s
             m  =  n * 2
         in sort (s ++ show m) == "123456789"

answer :: Int
answer = let s = head $ filter isOk xs
             m = read s * (2::Int)
         in read $ s ++ show m
            
main = print answer