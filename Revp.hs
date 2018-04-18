module Revp (revp) where

import Data.List (sortBy)
import Data.Monoid ((<>))
import Util (complement, parsePairs)

input :: String
input = ">Rosalind_24\nTCAATGCATGCGGGTCTATATGCAT"

revp :: String
revp =
  compute dna
  where
    dna = snd $ (\p -> p !! 0) $ parsePairs input

compute :: String -> String
compute dna =
  unlines
    $ map (\sub -> unwords [show $ position sub, show $ len sub])
    $ sortBy (\a b -> compare (position a) (position b))
    $ filter (isReversePalindome . val) candidates
  where
    candidates :: [Substring]
    candidates = foldl (\out len -> out <> (allSubs 1 len dna)) [] [4..12]

isReversePalindome :: String -> Bool
isReversePalindome str =
  str == (map complement $ reverse str)

data Substring =
  Substring
    { position :: Int
    , len :: Int
    , val :: String
    }

allSubs :: Int -> Int -> String -> [Substring]
allSubs pos len s
  | length s >= len =
    (Substring { position = pos, len = len, val = take len s })
      : allSubs (pos + 1) len (tail s)
  | otherwise = []
