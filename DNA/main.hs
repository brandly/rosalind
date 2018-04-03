module Main where

input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"

main :: IO ()
main =
  putStrLn $ unwords $ map (\l -> show $ occurrences l input) "ACGT"

occurrences :: Char -> String -> Int
occurrences letter str =
  length $ filter (\l -> l == letter) str
