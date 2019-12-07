module Day05 (solve) where

import Intcode (readProgram, runProgram)

solve = do
  program <- readProgram <$> readFile "../input/day05.txt"
  let output1 = runProgram program [1]
  putStrLn "Part 1:"
  print $ head output1
  let output2 = runProgram program [5]
  putStrLn "Part 2:"
  print $ head output2
