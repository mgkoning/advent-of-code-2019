module Day17 (solve) where

import Intcode
import Data.Char (chr, ord)
import Data.List.Split (chunksOf)

solve = do
  program <- readProgram <$> readFile "../input/day17.txt"
  putStrLn "Part 1:"
  let output = map chr $ runProgram program []
  putStr output
  {- ... on "paper" ... -}
  let intersectionSum = sum $ map product $ chunksOf 2 [8, 24, 14, 48, 18, 46, 48, 28, 48, 22, 42, 22, 42, 20, 40, 18, 40, 10, 44, 8]
  print intersectionSum
  putStrLn "Part 2:"
  let mainRoutine = "A,B,A,B,C,C,B,A,C,A"
      funA = "L,10,R,8,R,6,R,10"
      funB = "L,12,R,8,L,12"
      funC = "L,10,R,8,R,8"
      input = map ord $ unlines [mainRoutine, funA, funB, funC, "n\n"] {- Bad idea to turn video feed on -}
  let output2 = runProgram (memSet program 0 2) input
  print $ last output2