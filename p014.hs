-- ghc -rtsopts -O3 p014.hs
-- ./p014 +RTS -K100M -RTS
import Control.Monad.State
import qualified Data.Map as Map

type MM a = State (Map.Map Int Int) a

lookupMemo :: Int -> MM (Maybe Int)
lookupMemo i = state $ \m -> (Map.lookup i m, m)

writeMemo :: Int -> Int -> MM ()
writeMemo i n = state $ \m -> ((), Map.insert i n m)

f :: Int -> MM Int
f x | x == 1    = return 1
    | otherwise = do v <- lookupMemo x
                     case v of
                       Just n  -> return n
                       Nothing -> do let y | even x    = x `div` 2
                                           | otherwise = 3*x + 1
                                     l <- f y
                                     return (l+1)

len x = case runState (f x) (Map.empty) of (v, _) -> v
                
answer = foldr (\(p, l) (mp, ml) -> if l > ml then (p, l) else (mp, ml))
               (1, 1)
               (zip [1..] $ map len [1..1000000]  :: [(Int, Int)])

main = print $ case answer of (p, _) -> p

