{
module Main where
import Data.Char
import Data.List
}

%wrapper "basic"

tokens :-

\" [A-Z]* \" \, { \(_:s) -> (init.init) s }
\" [A-Z]* \"    { \(_:s) -> init s }

{
nameScore n = sum $ map (\c -> ord c - ord 'A' + 1) n

main = do
  s <- getContents
  let names   = sort (alexScanTokens s)
      scores  = map nameScore names
      scores' = zipWith (*) [1..] scores
      answer  = sum scores'
  print answer
}
