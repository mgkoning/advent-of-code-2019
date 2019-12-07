module Day07 (solve) where

import qualified Data.Vector.Unboxed as V
import Intcode (readProgram, runProgram, Input, IntermediateState(..), Suspended(..), runWithSuspend)
import Data.List (permutations, maximum)
import Data.Either (fromLeft, fromRight, isRight)

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

runFeedbackPermutations program =
  let phases = permutations [5..9]
      outputs = map (runFeedbackAmplifier program) phases
  in maximum outputs

runFeedbackAmplifier program phases =
  let amps = map (runWithSuspend program) $ map (:[]) phases
      runCycle :: [IntermediateState] -> Input -> [IntermediateState]
      runCycle (ampA:ampB:ampC:ampD:ampE:_) inputA =
        let runAmp name amp input = (resume $ fromLeft (error ("Amp " ++ name ++ " not suspended")) $ istate amp) input
            getOutput amp = take 1 $ either currentOutput id $ istate amp
            ampA' = runAmp "A" ampA inputA
            ampB' = runAmp "B" ampB $ getOutput ampA'
            ampC' = runAmp "C" ampC $ getOutput ampB'
            ampD' = runAmp "D" ampD $ getOutput ampC'
            ampE' = runAmp "E" ampE $ getOutput ampD'
            amps' = ampA':ampB':ampC':ampD':ampE':[]
        in if isRight $ istate ampE' then amps' else runCycle amps' $ getOutput ampE'
      (_:_:_:_:stateE:_) = runCycle amps [0]
      finalOutputE = head $ fromRight (error "Program not completed") $ istate stateE
  in finalOutputE

solve = do
  program <- readProgram <$> readFile "../input/day07.txt"
  putStrLn "Part 1:"
  print $ runAmplifierPermutations program
  putStrLn "Part 2:"
  print $ runFeedbackPermutations program
