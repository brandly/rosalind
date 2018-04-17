module Tran (tran) where

import Data.List (intercalate)
import Gc (parsePairs)

input :: String
input = ">Rosalind_0209\nGCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGA\nAGTACGGGCATCAACCCAGTT\n>Rosalind_2200\nTTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGC\nGGTACGAGTGTTCCTTTGGGT"

tran :: Double
tran =
  ratio
    $ transitionsTransversions (dna !! 0) (dna !! 1)
  where
    dna = map snd $ parsePairs input

ratio :: (Int, Int) -> Double
ratio (a, b) = fromIntegral a / fromIntegral b

transitionsTransversions :: String -> String -> (Int, Int)
transitionsTransversions a b =
  (itions, versions)
  where
    itions = length $ filter isTransition $ zip a b
    versions = length $ filter isTransversion $ zip a b

isTransition :: (Char, Char) -> Bool
isTransition ('A', 'G') = True
isTransition ('G', 'A') = True
isTransition ('C', 'T') = True
isTransition ('T', 'C') = True
isTransition _ = False

isTransversion :: (Char, Char) -> Bool
isTransversion (a, b) = a /= b && (not $ isTransition (a, b))
