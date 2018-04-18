module Gc (gc) where

import Data.List (intercalate)
import Util (parsePairs)

input = ">Rosalind_6404\nCCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC\nTCCCACTAATAATTCTGAGG\n>Rosalind_5959\nCCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT\nATATCCATTTGTCAGCAGACACGC\n>Rosalind_0808\nCCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC\nTGGGAACCTGCGGGCAGTAGGTGGAAT"

gc :: String
gc =
  intercalate "\n" [fst bigPair, show $ (*100) $ snd bigPair]
  where
    bigPair :: (String, Double)
    bigPair =
      foldl (\a b -> if snd b > snd a then b else a) ("", 0)
        $ map (\(id, str) -> (id, gcContent str))
        $ parsePairs input

gcContent :: String -> Double
gcContent str =
  (doubleLen $ filter (\c -> c == 'C' || c == 'G') str) / doubleLen str
  where
    doubleLen :: String -> Double
    doubleLen = fromIntegral . length
