import ForPrjEuler

loop n d th =
  let nds = length $ divisors n
  in if nds > th then n else loop (n+d) (d+1) th

answer = loop 1 2 500

main = print $ answer

