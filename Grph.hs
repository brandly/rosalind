module Grph (grph) where

import Util (parsePairs, lastN)

input :: String
input = ">Rosalind_0498\nAAATAAA\n>Rosalind_2391\nAAATTTT\n>Rosalind_2323\nTTTTCCC\n>Rosalind_0442\nAAATCCC\n>Rosalind_5013\nGGGTGGG"

k :: Int
k = 3

grph :: String
grph =
  unlines
    $ map (\(a, b) -> unwords [fst a, fst b])
    $ [(a, b)
      | a <- pairs
      , b <- pairs
      , a /= b
      , lastN k (snd a) == take k (snd b)
    ]
  where
    pairs :: [(String, String)]
    pairs = parsePairs input
