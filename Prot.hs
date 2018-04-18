module Prot (prot) where

import Data.List (intercalate)
import Util (codonToAmino, splitLen)

input :: String
input = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"

prot :: String
prot =
  intercalate ""
    $ filter (/="Stop")
    $ map codonToAmino
    $ splitLen 3 input
