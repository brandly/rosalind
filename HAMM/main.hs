module Main where

input = "GAGCCTACTAACGGGAT\nCATCGTAATGACGGCCT"

main :: IO ()
main =
  putStrLn
    $ show
    $ length
    $ filter (\(a, b) -> a /= b)
    $ zip (words input !! 0) (words input !! 1)
