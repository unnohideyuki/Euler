-- 2015-03-11
module Main where

main :: IO ()
main  =
  print $ sum [3,6..999] + sum [5,10..999] - sum [15,30..999]
