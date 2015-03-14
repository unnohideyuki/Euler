import Debug.Trace

n :: Integer
n  = read $ take 999 $ repeat '9'

-- fib' returns fibonacci term number that is bigger than th
fib' :: Integer -> Integer -> Int -> Integer -> Int
fib' a b i th = let x = a + b in
  if x > th
  then i
  else fib' b x (i+1) th

answer = fib' 1 1 3 n

main = print answer
