corners num siz = let d = siz - 1 in take 4 [num+d, num+d*2..]

ns = [1] ++ f 1 3
  where f n s | s > 1001   = []
              | otherwise = let xs = corners n s
                                n' = last xs
                            in xs ++ f n' (s+2)
answer = sum ns

main = print answer
