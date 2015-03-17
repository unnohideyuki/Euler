import Data.List.Split

-- same solver as p018
reduce _          []     xs = xs
reduce []         _      xs = xs
reduce (a1:a2:as) (b:bs) xs = reduce (a2:as) bs (xs ++ [b + max a1 a2])

solve ds = let as' = head ds
               ds' = tail ds
               
               loop as []       = as
               loop as (bs:bss) = loop cs bss
                 where cs = reduce as bs []
           in head $ loop as' ds'

readProblem xs = do
  s <- getContents
  let ls = init $ splitOn "\n" s
      f xs = (map read $ splitOn " " xs) :: [Int]
  return $ map f ls

main = do
  prob <- readProblem []
  print $ solve $ reverse prob
  