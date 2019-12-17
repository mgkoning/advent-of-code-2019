module Day17 (solve) where

import Intcode
import Data.Char (chr, ord)

solve = do
  program <- readProgram <$> readFile "../input/day17.txt"
  putStrLn "Part 1:"
  let output = map chr $ runProgram program []
  putStrLn output
  {- ... on "paper" ... -}
  putStrLn "Part 2:"
  let mainRoutine = "A,B,A,B,C,C,B,A,C,A"
      funA = "L,10,R,8,R,6,R,10"
      funB = "L,12,R,8,L,12"
      funC = "L,10,R,8,R,8"
      input = map ord $ unlines [mainRoutine, funA, funB, funC, "n\n"]
  let output2 = runProgram (memSet program 0 2) input
  print $ last output2