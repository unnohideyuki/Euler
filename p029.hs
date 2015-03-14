import qualified Data.Set as Set

answer = Set.size $ Set.fromList [pow a b | a <- [2..100], b <- [2..100]]
  where pow a b = product $ take b $ repeat a

main = print answer
