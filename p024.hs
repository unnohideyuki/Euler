import Data.List

findNth 0 xs as = as ++ xs
findNth n [] as = error "xs == []"
findNth n xs as = let np = product [1..(length xs - 1)]
                      n' = n `div` np
                      c  = xs !! n'
                  in findNth (n - n' * np) (xs \\ [c]) (as ++ [c])

answer = concat $ map show $ findNth 999999 [0..9] []

main = putStrLn answer

