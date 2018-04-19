module Prtm (prtm) where

import Data.Maybe (fromMaybe)

input = "SKADYEK"

prtm :: Double
prtm =
  foldl
    (\total c -> (+) total . fromMaybe 0 . lookup c $ massTable)
    (0 :: Double)
    input

massTableRaw = "A   71.03711\nC   103.00919\nD   115.02694\nE   129.04259\nF   147.06841\nG   57.02146\nH   137.05891\nI   113.08406\nK   128.09496\nL   113.08406\nM   131.04049\nN   114.04293\nP   97.05276\nQ   128.05858\nR   156.10111\nS   87.03203\nT   101.04768\nV   99.06841\nW   186.07931\nY   163.06333 "
massTable =
  map ((\pair -> (head . head $ pair , read $ pair !! 1 :: Double)) . words)
    $ lines
    $ massTableRaw
