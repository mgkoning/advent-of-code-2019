module Day21 (solve) where

import Data.Char (ord, chr)
import Intcode

checkOutput o = foldr (++) [] $ map safeChr o
  where safeChr c = if 255 < c then show c else [chr c]

doWalk p i = runProgram p $ map ord $ unlines i ++ "WALK\n"
doRun p i = runProgram p $ map ord $ unlines i ++ "RUN\n"

solve = do
  springScript <- readProgram <$> readFile "../input/day21.txt"
  putStrLn "Part 1:"
  let instructionsP1 = [
        "NOT A T",
        "OR T J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J"]
      output = doWalk springScript instructionsP1
  putStrLn $ checkOutput output
  putStrLn "Part 2:"
  let instructionsP2 = [
        "NOT A T",
        "OR T J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "AND E T",
        "OR H T",
        "AND T J"]
      output = doRun springScript instructionsP2
  putStrLn $ checkOutput output
 