module Cons (cons) where

import Data.List (intercalate)
import Gc (parsePairs)

input :: String
input = ">Rosalind_1\nATCCAGCT\n>Rosalind_2\nGGGCAACT\n>Rosalind_3\nATGGATCT\n>Rosalind_4\nAAGCAACC\n>Rosalind_5\nTTGGAACT\n>Rosalind_6\nATGCCATT\n>Rosalind_7\nATGGCACT"

cons :: String
cons =
  unlines $ [consensus finalCounts, table finalCounts]
  where
    finalCounts :: [[(Char, Int)]]
    finalCounts = foldl update initCounts values

    update :: [[(Char, Int)]] -> String -> [[(Char, Int)]]
    update counts str =
      map (uncurry increment) $ zip counts str

    initCounts :: [[(Char, Int)]]
    initCounts = map (\_ -> []) (values !! 0)

    values :: [String]
    values = map snd $ parsePairs input

table :: [[(Char, Int)]] -> String
table counts =
  unlines
    $ map (\(c, ints) -> unwords $ (c : ":") : (map show ints))
    $ map (\c -> (c, forChar c)) "ACGT"
  where
    forChar :: Char -> [Int]
    forChar c =
      map
        (\cnt ->
          case lookup c cnt of
            Just num -> num
            Nothing -> 0
        )
        counts

consensus :: [[(Char, Int)]] -> String
consensus counts =
  map (fst . max) counts
  where
    max :: [(Char, Int)] -> (Char, Int)
    max = foldl (\a b -> if snd b > snd a then b else a) (' ', 0)

increment :: [(Char, Int)] -> Char -> [(Char, Int)]
increment list c =
  case lookup c list of
    Just num ->
      (c, num + 1) : filter (\(char, _) -> char /= c) list
    Nothing ->
      (c, 1) : list
