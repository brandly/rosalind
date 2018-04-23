-- module Lexv (lexv) where
module Main where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

main :: IO ()
main = putStrLn lexv

input :: String
input = "D N A\n3"

lexv :: String
lexv =
  unlines
    $ sortBy (lexic alphabet)
    $ go n alphabet (map (\c -> [c]) alphabet)
  where
    alphabet :: [Char]
    alphabet = fst . parse $ input

    n :: Int
    n = snd . parse $ input

    go :: Int -> [Char] -> [String] -> [String]
    go 1 _ prior = prior
    go n alpha prior =
      prior <> (
        go (n - 1) alpha $ [p <> [c] | p <- prior, c <- alpha]
      )

lexic :: [Char] -> String -> String -> Ordering
lexic _ [] b = LT
lexic _ a [] = GT
lexic alpha a b =
  if comp == EQ then
    lexic alpha (tail a) (tail b)
  else
    comp
  where
    comp :: Ordering
    comp =
      compare
        (elemIndex (head a) alpha)
        (elemIndex (head b) alpha)

parse :: String -> ([Char], Int)
parse str =
  ( filter ((/=) ' ') $ takeWhile ((/=) '\n') str
  , read $ drop 1 $ dropWhile ((/=) '\n') str :: Int
  )
