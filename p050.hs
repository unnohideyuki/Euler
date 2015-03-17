import ForPrjEuler
import Debug.Trace

check len ps = if len > length ps
               then Nothing
               else if isPrime p then Just p else check len ps'
                 where p = sum $ take len ps
                       ps' = tail ps

findAnswer l = trace (show l) $ case check l primes of
  Just p  -> p
  Nothing -> findAnswer (l-1)

-- We can start from 546 becase sum of first 547 primes > 1000000
answer = findAnswer 546

main = print answer


