module Dna (dna) where

input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"

dna :: String
dna =
  unwords $ map (\l -> show $ occurrences l input) "ACGT"

occurrences :: Char -> String -> Int
occurrences letter str =
  length $ filter (\l -> l == letter) str
