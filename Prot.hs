module Prot (prot) where

import Data.List (intercalate)

import qualified Data.Map as M (Map, fromList, lookup)

input :: String
input = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"

prot :: String
prot =
  intercalate ""
    $ filter (/="Stop")
    $ map codonToAmino
    $ splitLen 3 input

codonToAmino :: String -> String
codonToAmino codon =
  case M.lookup codon rnaCodonTable of
    Just val -> val
    Nothing -> ""

rnaCodonTable :: M.Map String String
rnaCodonTable =
  M.fromList
    $ listOfTuples
    -- newlines removed by hand
    $ words "UUU F      CUU L      AUU I      GUU V   UUC F      CUC L      AUC I      GUC V   UUA L      CUA L      AUA I      GUA V   UUG L      CUG L      AUG M      GUG V   UCU S      CCU P      ACU T      GCU A   UCC S      CCC P      ACC T      GCC A   UCA S      CCA P      ACA T      GCA A   UCG S      CCG P      ACG T      GCG A   UAU Y      CAU H      AAU N      GAU D   UAC Y      CAC H      AAC N      GAC D   UAA Stop   CAA Q      AAA K      GAA E   UAG Stop   CAG Q      AAG K      GAG E   UGU C      CGU R      AGU S      GGU G   UGC C      CGC R      AGC S      GGC G   UGA Stop   CGA R      AGA R      GGA G   UGG W      CGG R      AGG R      GGG G "

listOfTuples :: [String] -> [(String, String)]
listOfTuples [] = []
listOfTuples (k:v:t) = (k, v) : listOfTuples t

splitLen :: Int -> [a] -> [[a]]
splitLen len str =
  if length str >= len then
    take len str : (splitLen len $ (iterate tail str !! len))
  else
    []
