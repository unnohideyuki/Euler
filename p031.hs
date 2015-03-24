ways = [ 1 | twoPounds <- [0..2],
             pounds    <- [0..(rest twoPounds 0      0   0   0   0  `div` 100)],
             p50       <- [0..(rest twoPounds pounds 0   0   0   0  `div` 50)],
             p20       <- [0..(rest twoPounds pounds p50 0   0   0  `div` 20)],
             p10       <- [0..(rest twoPounds pounds p50 p20 0   0  `div` 10)],
             p5        <- [0..(rest twoPounds pounds p50 p20 p10 0  `div` 5)],
             p2        <- [0..(rest twoPounds pounds p50 p20 p10 p5 `div` 2)]]
  where rest c1 c2 c3 c4 c5 c6 =
          200 - 200*c1 - 100*c2 - 50*c3 - 20*c4 -10*c5 - 5*c6 
                         
answer = sum ways

main = print answer
