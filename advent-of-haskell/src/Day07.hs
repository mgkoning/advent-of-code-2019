module Day07 (solve) where

import qualified Data.Vector.Unboxed as V
import Intcode (
  readProgram, runProgram, Input
  )
import Data.List (permutations, maximum)
import Data.Either (fromLeft, fromRight, isRight)

runFeedbackPermutations program phases chooseOutput =
  let outputs = map (chooseOutput . runFeedbackAmplifier program) $ permutations phases
  in maximum outputs

runFeedbackAmplifier program (phaseA:phaseB:phaseC:phaseD:phaseE:_) =
  let ampA = runProgram program (phaseA: 0: ampE)
      ampB = runProgram program (phaseB: ampA)
      ampC = runProgram program (phaseC: ampB)
      ampD = runProgram program (phaseD: ampC)
      ampE = runProgram program (phaseE: ampD)
  in ampE

solve = do
  program <- readProgram <$> readFile "../input/day07.txt"
  putStrLn "Part 1:"
  print $ runFeedbackPermutations program [0..4] head
  putStrLn "Part 2:"
  print $ runFeedbackPermutations program [5..9] last
