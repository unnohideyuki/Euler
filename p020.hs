import Data.Char

answer = sum $ map (\c -> ord c - ord '0') $ show $ product [1..100]

main = print answer
