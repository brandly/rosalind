module Sign (sign) where

sign :: String
sign =
  unlines (
    (show $ length results)
      : map (unwords . (map show)) results
  )
  where
    results = signedPerms 2

signedPerms :: Int -> [[Int]]
signedPerms n =
  go vals
  where
    vals = [1..n]

    -- doesn't feel super graceful
    go :: [Int] -> [[Int]]
    go [] = []
    go [a] = [[a], [-a]]
    go vals =
      mconcat $
        map
          (\v ->
            mconcat $
              map
                (\rest -> [v : rest, -v : rest])
                (go $ filter ((/=) v) vals)
          )
          vals
