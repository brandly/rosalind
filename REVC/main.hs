module Main where

input = "AAAACCCGGT"

main :: IO ()
main =
  putStrLn $ map complement $ reverse input

complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'
complement _   = undefined
