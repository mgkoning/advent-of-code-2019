{-# LANGUAGE RecordWildCards #-}

module Intcode (runSimple, Program, Input, Output, runProgram) where

import qualified Data.Vector.Unboxed as V

type Program = V.Vector Int
type Input = [Int]
type Output = [Int]
data State = State { input :: Input, program :: Program, output :: Output }

runSimple :: Program -> Program
runSimple program = finalProgram
  where State { program = finalProgram } = doRunProgram (State [] program []) 0

runProgram :: Program -> Input -> Output
runProgram program input = output
  where (State {output = output}) = doRunProgram (State input program []) 0

doRunProgram :: State -> Int -> State
doRunProgram state@(State {..}) pointer =
  let current = program V.! pointer
      (argModes, opcode) = current `divMod` 100
      (argModes', arg1Mode) = argModes `divMod` 10
      (argModes'', arg2Mode) = argModes' `divMod` 10
      (_, arg3Mode) = argModes'' `divMod` 10
      (headInput:input') = input
      arg1 = let i = pointer + 1 in if arg1Mode == 0 then program V.! i else i
      arg1Val = program V.! arg1
      arg2 = let i = pointer + 2 in if arg2Mode == 0 then program V.! i else i
      arg2Val = program V.! arg2
      arg3 = let i = pointer + 3 in if arg3Mode == 0 then program V.! i else i
  in case opcode of
      1 -> doRunProgram state { program = (program V.// [(arg3, (arg1Val + arg2Val))]) } (pointer + 4)
      2 -> doRunProgram state { program = (program V.// [(arg3, (arg1Val * arg2Val))]) } (pointer + 4)
      3 -> doRunProgram (State input' (program V.// [(arg1, headInput)]) output) (pointer + 2)
      4 -> doRunProgram state { output = arg1Val:output } (pointer + 2)
      5 -> doRunProgram state (if arg1Val /= 0 then arg2Val else pointer + 3)
      6 -> doRunProgram state (if arg1Val == 0 then arg2Val else pointer + 3)
      7 -> doRunProgram state { program = (program V.// [(arg3, if arg1Val < arg2Val then 1 else 0)]) } (pointer + 4)
      8 -> doRunProgram state { program = (program V.// [(arg3, if arg1Val == arg2Val then 1 else 0)]) } (pointer + 4)
      99 -> state
      x -> error ("unknown opcode " ++ (show x))
