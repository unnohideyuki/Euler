import Data.List

main = print $  last $ sort [x*y | x <- [100..999], y <- [100..999], isPalindrome $ show (x*y)]
  where isPalindrome []  = True
        isPalindrome [_] = True
        isPalindrome (c:cs) = if c /= last cs 
                              then False 
                              else isPalindrome (init cs)

   
