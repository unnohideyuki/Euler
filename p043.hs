import Data.List

d8d9d10 = filter g $ map f [17, 17*2..987]
  where f i | i < 100   = '0' : show i
            | otherwise = show i
        g s = (s == nub s)
        
appendDigit ss divisor = [s | x <- ss
                            , let ds = "0123456789" \\ x
                            , d <- ds
                            , let s = d:x
                            , (read $ take 3 s :: Int) `mod` divisor == 0]

d7d8d9d10 = appendDigit d8d9d10 13
d6d7d8d9d10 = appendDigit d7d8d9d10 11
d5d6d7d8d9d10 = appendDigit d6d7d8d9d10 7
d4d5d6d7d8d9d10 = appendDigit d5d6d7d8d9d10 5
d3d4d5d6d7d8d9d10 = appendDigit d4d5d6d7d8d9d10 3
d2d3d4d5d6d7d8d9d10 = appendDigit d3d4d5d6d7d8d9d10 2
d1d2d3d4d5d6d7d8d9d10 = appendDigit d2d3d4d5d6d7d8d9d10 1

answer = sum [n | s <- d1d2d3d4d5d6d7d8d9d10
                , head s /= '0'
                , let n = read s :: Int]
         
main = print answer


