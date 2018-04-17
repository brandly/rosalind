module Hamm (hamm) where

input = "GAGCCTACTAACGGGAT\nCATCGTAATGACGGCCT"

hamm :: Int
hamm =
  length
    $ filter (\(a, b) -> a /= b)
    $ zip (words input !! 0) (words input !! 1)
