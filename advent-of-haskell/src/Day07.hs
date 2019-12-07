module Day07 (solve) where

import qualified Data.Vector.Unboxed as V
import Intcode (runProgram)
import Parsing (parseCommaSeparated, parseInt, resultOrError)
import Data.List (permutations, maximum)

runAmplifierPermutations program =
  let phases = permutations [0..4]
      outputs = map (runAmplifier program) phases
  in maximum outputs

runAmplifier program phases =
  let phaseA:phaseB:phaseC:phaseD:phaseE:_ = phases
      outputsA = runProgram program [phaseA, 0]
      outputsB = runProgram program [phaseB, head $ outputsA]
      outputsC = runProgram program [phaseC, head $ outputsB]
      outputsD = runProgram program [phaseD, head $ outputsC]
      outputsE = runProgram program [phaseE, head $ outputsD]
  in head outputsE

solve = do
  program <- V.fromList <$>
             resultOrError <$> parseCommaSeparated parseInt <$>
             readFile "../input/day07.txt"
  putStrLn "Part 1:"
  print $ runAmplifierPermutations program
