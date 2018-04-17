module Rna (rna, toRna) where

input = "GATGGAACTTGACTACGTAAATT"

rna :: String
rna = toRna input

toRna :: String -> String
toRna = map (\l -> if l == 'T' then 'U' else l)
