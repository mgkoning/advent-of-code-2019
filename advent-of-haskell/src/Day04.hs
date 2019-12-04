module Day04 (solve) where

import Data.List (group)

isValidPassword strict code = neverDecreasing && repeated
  where text = show code
        neverDecreasing = all (uncurry (<=)) $ zip text (drop 1 text)
        op = if strict then (==) else (>=)
        repeated = any ((`op` 2) . length) $ group text


solve = do
  putStrLn "Part 1:"
  let from = 236491
      to = 713787
      valid1 = filter (isValidPassword False) [from..to]
  print $ length valid1
  putStrLn "Part 1:"
  let valid2 = filter (isValidPassword True) [from..to]
  print $ length valid2