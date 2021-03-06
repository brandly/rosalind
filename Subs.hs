module Subs (subs) where

input = "GATATATGCATATACTT\nATAT"

subs :: String
subs =
  unwords
    $ map (show . fst)
    $ filter ((== needle) . snd)
    $ indexedSubstrings (length needle) haystack
  where
    haystack = words input !! 0
    needle = words input !! 1

indexedSubstrings :: Int -> String -> [(Int, String)]
indexedSubstrings len str =
  map (\i -> (i + 1, take len $ drop i str)) [0..length str]
