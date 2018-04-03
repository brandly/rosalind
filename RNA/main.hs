module Main where

input = "GATGGAACTTGACTACGTAAATT"

main :: IO ()
main =
  putStrLn $ map (\l -> if l == 'T' then 'U' else l) input
