s = sum [product $ take x' $ repeat x | x  <- [1..(1000::Integer)],
                                        let x' = fromIntegral x]

answer = reverse $ take 10 $ reverse $ show s

main = putStrLn answer

