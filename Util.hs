module Util (
  toRna,
  parsePairs,
  listOfTuples,
  splitLen,
  codonToAmino,
  complement,
  lastN
) where

import Data.List (intercalate)
import qualified Data.Map as M (Map, fromList, lookup)


toRna :: String -> String
toRna = map (\l -> if l == 'T' then 'U' else l)


parsePairs :: String -> [(String, String)]
parsePairs str =
  go $ lines str
  where
    go :: [String] -> [(String, String)]
    go [] = []
    go (key:lns) =
      (drop 1 key, intercalate "" $ takeWhile (not . startsWith '>') lns)
        : (go $ dropWhile (not . startsWith '>') lns)

startsWith :: Char -> String -> Bool
startsWith c str =
  str !! 0 == c


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


complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'
complement _   = undefined


lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs
