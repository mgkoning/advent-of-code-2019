module Day21 (solve) where

import Data.Char (ord, chr)
import Intcode


checkOutput o = foldr (++) [] $ map safeChr o
  where safeChr c = if 255 < c then show c else [chr c]

doWalk p i = runProgram p $ map ord $ unlines i ++ "WALK\n"

solve = do
  springScript <- readProgram <$> readFile "../input/day21.txt"
  putStrLn "Part 1:"
  let instructions = [
        "NOT A T",
        "OR T J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J"]
      output = doWalk springScript instructions
  putStrLn $ checkOutput output