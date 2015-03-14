main = print (b - a)
  where a  = sum [x*x | x <- [1..100]]
        b' = sum [1..100]
        b  = b' * b'