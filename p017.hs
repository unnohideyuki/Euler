import Debug.Trace

nLetters x | x == 0 = error "do not call for 00"
           | x == 1 = length "one"
           | x == 2 = length "two"
           | x == 3 = length "three"
           | x == 4 = length "four"
           | x == 5 = length "five"
           | x == 6 = length "six"
           | x == 7 = length "seven"
           | x == 8 = length "eight"
           | x == 9 = length "nine"
           | x == 10 = length "ten"
           | x == 11 = length "eleven"
           | x == 12 = length "twelve"
           | x == 13 = length "thirteen"
           | x == 14 = length "fourteen"
           | x == 15 = length "fifteen"
           | x == 16 = length "sixteen"
           | x == 17 = length "seventeen"
           | x == 18 = length "eighteen"
           | x == 19 = length "nineteen"
           | x == 20 = length "twenty"
           | x == 30 = length "thirty"
           | x == 40 = length "forty"
           | x == 50 = length "fifty"
           | x == 60 = length "sixty"
           | x == 70 = length "seventy"
           | x == 80 = length "eighty"
           | x == 90 = length "ninety"
           | x < 100  = let d = x `mod` 10 in nLetters (x-d) + nLetters d
           | x < 1000 = nLetters (x `div` 100) + 
                        length "hundred" + 
                        if (x `mod` 100) /= 0 then length "and" + nLetters (x `mod` 100) else 0
           | x == 1000 = length "onethousand"

answer = sum $ map nLetters [1..1000]

main = print answer

                            
                      