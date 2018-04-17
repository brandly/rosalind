{-# LANGUAGE OverloadedStrings #-}

module Splc (splc) where

import Data.List (intercalate)
import Gc (parsePairs)
import Rna (toRna)
import Prot (listOfTuples, splitLen, codonToAmino)
import qualified Data.Text as T
import qualified Data.Map as M (Map, fromList, lookup)

input :: String
input = ">Rosalind_10\nATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG\n>Rosalind_12\nATCGGTCGAA\n>Rosalind_15\nATCGGTCGAGCGTGT"

splc :: String
splc =
  intercalate ""
    $ filter (/="Stop")
    $ map codonToAmino
    $ splitLen 3
    $ toRna
    $ foldl strip dna introns
  where
    dna = head $ map snd $ parsePairs input
    introns = tail $ map snd $ parsePairs input

strip :: String -> String -> String
strip str sub =
  -- seems unnecessary to go thru Text
  intercalate "" $ map T.unpack $ T.splitOn (T.pack sub) (T.pack str)
