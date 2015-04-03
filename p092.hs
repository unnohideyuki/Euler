import Data.Char

chain' n = let s  = show n
               ns = map (\c -> ord c - ord '0') s
           in sum $ map (\x -> x^2) ns
              
arrive89 n = case chain' n of
  1  -> False
  89 -> True
  n' -> arrive89 n'
  
answer = sum [1 | x <- [2..10000000], arrive89 x]

main = print answer

              
              