-- 2015-03-12
f acc a b x | a + b > x = acc
            | otherwise     = f acc' b (a+b) x
              where acc' = acc + if even (a+b) 
                                 then a+b 
                                 else 0
                                    
                         
main = print $ f 0 1 1 4000000




    