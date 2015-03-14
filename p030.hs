import Data.Char

isOk n = n == n'
  where n' = sum $ map f $ show n
        f c = let x = ord c - ord '0' 
              in x * x * x * x * x
                 
-- TODO: find the appropriate upper bound of x below                 
answer = sum [x | x <- [2..1000000], isOk x]

main = print answer