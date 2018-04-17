module Perm (perm) where

import Data.List (permutations)

perm :: String
perm =
  unlines $ header : map unwords perms
  where
    n = 3

    header :: String
    header = show $ length perms

    perms :: [[String]]
    perms = permutations $ map show [1..n]
