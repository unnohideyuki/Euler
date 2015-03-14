import Data.Char

d n = ord (digits !! (n - 1)) - ord '0'
  where digits = concat $ fmap show [1..]
  
answer = d 1 * d 10 * d 100 * d 1000 * d 10000 * d 100000 * d 1000000

main = print answer
