import Data.List
import Data.Numbers.Primes

ys :: [Int]
ys = [x | let d9 = ['7', '6'..'1']
        , a <- d9 , let d8 = d9 \\ [a]
        , b <- d8 , let d7 = d8 \\ [b]
        , c <- d7 , let d6 = d7 \\ [c]
        , d <- d6 , let d5 = d6 \\ [d]
        , e <- d5 , let d4 = d5 \\ [e]
        , f <- d4 , let d3 = d4 \\ [f]
        , g <- d3 
        , let x = read $ [a, b, c, d, e, f, g]]
     

answer = head $ filter isPrime ys

main = print answer


                        