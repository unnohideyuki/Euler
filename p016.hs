import Data.Char
n = product $ take 1000 $ repeat 2
answer = sum $ map (\c -> ord c - ord '0') $ show n
main = print answer