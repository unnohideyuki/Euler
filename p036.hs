main = print answer

answer =  sum [x | x <- [1..1000000], isPalindrome $ show x, isPalindrome $ i2b x]
  where isPalindrome []  = True
        isPalindrome [_] = True
        isPalindrome (c:cs) = if c /= last cs 
                              then False 
                              else isPalindrome (init cs)
        i2b x = i2b' x ""
        i2b' 0 s = s
        i2b' x s = case x `mod` 2 of 
          0 -> i2b' (x `div` 2) ('0':s)
          1 -> i2b' (x `div` 2) ('1':s)
   
