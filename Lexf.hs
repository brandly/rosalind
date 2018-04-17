module Lexf (lexf) where

lexf :: String
lexf = unlines $ combos 2 "AGCT"

combos :: Int -> String -> [String]
combos 0 _ = []
combos 1 str = map (\char -> [char]) str
combos n str =
  concat $ map
    (\char ->
      map
        (char:)
        (combos (n - 1) str)
    )
    str
