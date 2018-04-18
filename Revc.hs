module Revc (revc) where

import Util (complement)

input :: String
input = "AAAACCCGGT"

revc :: String
revc = map complement $ reverse input
