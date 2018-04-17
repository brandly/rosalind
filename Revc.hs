module Revc (revc) where

input :: String
input = "AAAACCCGGT"

revc :: String
revc = map complement $ reverse input

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'
complement _   = undefined
