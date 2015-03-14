import Control.Monad.State
import qualified Data.Map as Map

type MM a = State (Map.Map (Int, Int) Int) a

lookupMemo :: Int -> Int -> MM (Maybe Int)
lookupMemo i j = state $ \m -> (Map.lookup (i, j) m, m)

writeMemo :: Int -> Int -> Int -> MM ()
writeMemo i j n = state $ \m -> ((), Map.insert (i, j) n m)

nRoutes :: Int -> Int -> MM Int
nRoutes i j | i*j == 0  = return 1
            | otherwise = do v <- lookupMemo i j
                             case v of
                               Just n  -> return n
                               Nothing -> do n1 <- nRoutes (i-1) j
                                             n2 <- nRoutes i     (j-1)
                                             let n = n1 + n2
                                             writeMemo i j n
                                             return n
                                             
answer i j = case runState (nRoutes i j) (Map.empty) of (n, _) -> n
                                                        
main = print $ answer 20 20
              
