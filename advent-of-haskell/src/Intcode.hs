{-# LANGUAGE RecordWildCards #-}

module Intcode (runSimple, State(..), runProgram) where

import qualified Data.Vector.Unboxed as V

runSimple :: V.Vector Int -> V.Vector Int
runSimple program = finalProgram
  where State { program = finalProgram } = runProgram (State [] program []) 0

data State = State { input :: [Int], program :: V.Vector Int, output :: [Int] }

runProgram :: State -> Int -> State
runProgram state@(State {..}) pointer =
  let current = program V.! pointer
      (argModes, opcode) = current `divMod` 100
      (argModes', arg1Mode) = argModes `divMod` 10
      (argModes'', arg2Mode) = argModes' `divMod` 10
      (_, arg3Mode) = argModes'' `divMod` 10
      (headInput:input') = input
      arg1 = program V.! let i = pointer + 1 in if arg1Mode == 0 then program V.! i else i
      arg2 = program V.! let i = pointer + 2 in if arg2Mode == 0 then program V.! i else i
      arg3 = let i = pointer + 3 in if arg3Mode == 0 then program V.! i else i
  in case opcode of
      1 -> runProgram state { program = (program V.// [(arg3, (arg1 + arg2))]) } (pointer + 4)
      2 -> runProgram state { program = (program V.// [(arg3, (arg1 * arg2))]) } (pointer + 4)
      3 -> runProgram (State input' (program V.// [(arg3, headInput)]) output) (pointer + 2)
      4 -> runProgram state { output = arg1:output } (pointer + 2)
      5 -> runProgram state (if arg1 /= 0 then arg2 else pointer + 3)
      6 -> runProgram state (if arg1 == 0 then arg2 else pointer + 3)
      7 -> runProgram state { program = (program V.// [(arg3, if arg1 < arg2 then 1 else 0)]) } (pointer + 4)
      8 -> runProgram state { program = (program V.// [(arg3, if arg1 == arg2 then 1 else 0)]) } (pointer + 4)
      99 -> state
      x -> error ("unknown opcode " ++ (show x))
