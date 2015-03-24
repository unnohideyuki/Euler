import qualified Data.Set as Set
import Data.List

s0 :: Set.Set Int
s0 = Set.empty

ss = [s | d1 <- [1..9]
        , d2 <- ([1..9] \\ [d1])
        , d3 <- ([1..9] \\ [d1, d2])
        , d4 <- ([1..9] \\ [d1, d2, d3])
        , d5 <- ([1..9] \\ [d1, d2, d3, d4])
        , let s = show d1 ++ show d2 ++ show d3 ++ show d4 ++ show d5]
     
chkPandigital s l = let n1 = (read $ take l s) :: Int
                        n2 = (read $ drop l s) :: Int
                        n  = n1 * n2
                        rest = ("123456789" \\ s) 
                        sn = show n
                        b  = (rest \\ sn == "") && (sn \\ rest == "")
                   in
                     (b, n)

findPandigitals set []     = Set.toList set
findPandigitals set (x:xs) = let
  (b1, n1) = chkPandigital x 1
  (b2, n2) = chkPandigital x 2
  
  set'  = if b1 then Set.insert n1 set  else set
  set'' = if b2 then Set.insert n2 set' else set'
  in
   findPandigitals set'' xs
                    
answer = sum $ findPandigitals s0 ss                   

main = print answer
