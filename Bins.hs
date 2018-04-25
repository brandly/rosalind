module Main where

import Data.List

input :: String
input = "5\n6\n10 20 30 40 50\n40 10 35 15 40 20"

main :: IO ()
main = putStrLn . unwords . (map show) . indexes $ parse input

indexes :: ([Int], [Int]) -> [Int]
indexes (a, b) =
  map go b
  where
    go :: Int -> Int
    go n =
      case elemIndex n a of
        Just i ->
          i + 1
        Nothing ->
          -1

parse :: String -> ([Int], [Int])
parse input =
  (toInts (theLines !! 2), toInts (theLines !! 3))
  where
    theLines = lines input

    toInts :: String -> [Int]
    toInts line =
      map read $ words line
