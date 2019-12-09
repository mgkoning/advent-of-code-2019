module Day09 (solve, test) where

import Intcode

solve = do
  program <- readProgram <$> readFile "../input/day09.txt"
  putStrLn "Part 1:"
  print $ runProgram program [1]
  putStrLn "Part 2:"
  print $ runProgram program [2]
  

test = do
  let program1 = readProgram $ "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
  print $ runProgram program1 []
  let program2 = readProgram $ "1102,34915192,34915192,7,4,7,99,0"
  print $ runProgram program2 []
  let program3 = readProgram $ "104,1125899906842624,99"
  print $ runProgram program3 []