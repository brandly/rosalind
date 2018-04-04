module Main where

import Data.List (intercalate)

input = ">Rosalind_6404\nCCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC\nTCCCACTAATAATTCTGAGG\n>Rosalind_5959\nCCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT\nATATCCATTTGTCAGCAGACACGC\n>Rosalind_0808\nCCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC\nTGGGAACCTGCGGGCAGTAGGTGGAAT"

main :: IO ()
main =
  putStrLn $ intercalate "\n" [fst bigPair, show $ (*100) $ snd bigPair]
  where
    bigPair :: (String, Double)
    bigPair =
      foldl (\a b -> if snd b > snd a then b else a) ("", 0)
        $ map (\(id, str) -> (id, gcContent str))
        $ parsePairs input

parsePairs :: String -> [(String, String)]
parsePairs str =
  go $ lines str
  where
    go :: [String] -> [(String, String)]
    go [] = []
    go (key:lns) =
      (drop 1 key, intercalate "" $ takeWhile (not . startsWith '>') lns)
        : (go $ dropWhile (not . startsWith '>') lns)

startsWith :: Char -> String -> Bool
startsWith c str =
  str !! 0 == c

gcContent :: String -> Double
gcContent str =
  (doubleLen $ filter (\c -> c == 'C' || c == 'G') str) / doubleLen str
  where
    doubleLen :: String -> Double
    doubleLen = fromIntegral . length
