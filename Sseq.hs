module Main where

import Data.List
import Data.Maybe (fromMaybe)
import Util (parsePairs)

input :: String
input = ">Rosalind_14\nACGAACGTGACG\n>Rosalind_18\nGTA"

main :: IO ()
main = putStrLn . unwords . map (show . ((+) 1)) $ compute big small
  where
    pairs = parsePairs input
    big = snd $ (pairs !! 0)
    small = snd $ (pairs !! 1)

compute :: String -> [Char] -> [Int]
compute big small =
  findEm big small 0 []
  where
    findEm :: String -> [Char] -> Int -> [Int] -> [Int]
    findEm _   []    _   sofar = sofar
    findEm big alpha pad sofar =
      findEm
        (drop (i + 1) big)
        (tail alpha)
        (pad + i + 1)
        (sofar ++ [pad + i])
      where
        i :: Int
        i = fromMaybe (-1) (elemIndex (head alpha) big)
