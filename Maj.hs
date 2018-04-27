module Main where

import Data.List
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

input :: String
input = "4 8\n5 5 5 5 5 5 5 5\n8 7 7 7 1 7 3 7\n7 1 6 5 10 100 1000 1\n5 1 6 7 1 1 10 1"

main :: IO ()
main = putStrLn . unwords . (map (show . maj)) $ parse input

maj :: [Int] -> Int
maj list =
  if (fromIntegral (snd maxCount)) > ((fromIntegral (length list)) / 2) then
    fst maxCount
  else
    -1
  where
    maxCount :: (Int, Int)
    maxCount = maximumBy (comparing snd) $ countVal

    countVal :: [(Int, Int)]
    countVal = Map.toList $ foldl fn Map.empty list

    fn :: Map.Map Int Int -> Int -> Map.Map Int Int
    fn m n =
      if Map.member n m then
        Map.adjust ((+) 1) n m
      else
        Map.insert n 1 m

parse :: String -> [[Int]]
parse input =
  map (\l -> map read $ words l) . tail $ lines input
