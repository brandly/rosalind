module Main where

input :: String
input = "AAAACCCGGT"

main :: IO ()
main =
  putStrLn $ map complement $ reverse input

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'
complement _   = undefined
