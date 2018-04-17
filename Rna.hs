module Rna (rna) where

input = "GATGGAACTTGACTACGTAAATT"

rna :: String
rna = map (\l -> if l == 'T' then 'U' else l) input
