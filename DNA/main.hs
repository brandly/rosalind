module Main where

input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"

main :: IO ()
main =
  putStrLn $ unwords $ map (\l -> show $ occurences l input) "ACGT"

occurences :: Char -> String -> Int
occurences letter str =
  length $ filter (\l -> l == letter) str
