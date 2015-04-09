import Data.Char

{-
      1         z
f = ------ = ------
    x + y    zx + y
        -
        z
-}
f :: Integer -> Integer -> Integer -> (Integer, Integer)
f x y z = let n = z
              d = x*z + y
              m = gcd n d
          in (n `div` m, d `div` m)

{-
         b    ca + b
g = a + --- = ------
         c       c
-}
g :: Integer -> (Integer, Integer) -> (Integer, Integer)
g a (b, c) = let n = c*a + b
                 d = c
                 m = gcd n d
             in (n `div` m, d `div` m)

ns = concat [[1, 2*k, 1] | k <- [1..]]

calcE n = let xs = take (n-1) ns
          in  g 2 $ calc' xs
  where calc' [] = (0, 1)
        calc' (x:[]) = (1, x)
        calc' (x:xs) = let (y, z) = calc' xs
                       in f x y z

answer = let (n, d) = calcE 100
         in sum $ map (\c -> ord c - ord '0') $ show n

main = print answer

