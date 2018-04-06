module Main where

import Data.List (permutations)

main :: IO ()
main =
  putStrLn $ unlines $ header : map unwords perms
  where
    n = 3

    header :: String
    header = show $ length perms

    perms :: [[String]]
    perms = permutations $ map show [1..n]
