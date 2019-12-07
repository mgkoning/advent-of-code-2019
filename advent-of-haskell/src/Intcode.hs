{-# LANGUAGE RecordWildCards #-}

module Intcode (
  readProgram,
  runSimple,
  Program, Input, Output, runProgram,
  IntermediateState(..), Suspended(..), runWithSuspend
) where

import qualified Data.Vector.Unboxed as V
import Parsing (parseCommaSeparated, parseInt, resultOrError)

type Program = V.Vector Int
type Input = [Int]
type Output = [Int]

data Suspended = Suspended { currentOutput :: Output, resume :: Input -> IntermediateState }
newtype IntermediateState = IntermediateState { istate :: Either Suspended Output }

data Mode = Running | WaitingForInput | Halted deriving (Eq, Show)
data State = State { input :: Input, program :: Program, mode :: Mode, ip :: Int, output :: Output }

readProgram :: String -> Program
readProgram = V.fromList . resultOrError . (parseCommaSeparated parseInt)

runSimple :: Program -> Program
runSimple program = if mode == Halted then finalProgram else error ("Did not run until halted, mode: " ++ show mode)
  where State { program = finalProgram, mode = mode } = doRunProgram (State [] program Running 0 [])

runProgram :: Program -> Input -> Output
runProgram program input = if mode == Halted then output else error ("Did not run until halted, mode: " ++ show mode)
  where (State {output = output, mode = mode}) = doRunProgram (State input program Running 0 [])

runWithSuspend :: Program -> Input -> IntermediateState
runWithSuspend program input = runWithSuspend' $ State input program Running 0 []
  where 
    runWithSuspend' s =
      let state@(State {output = output, mode = mode}) = doRunProgram s
      in case mode of
           Halted -> IntermediateState $ Right output
           WaitingForInput -> 
             let suspended = Suspended output $ \i -> runWithSuspend' state { input = i, mode = Running }
             in IntermediateState $ Left suspended

doRunProgram :: State -> State
doRunProgram state@(State {..}) =
  let current = program V.! ip
      (argModes, opcode) = current `divMod` 100
      (argModes', arg1Mode) = argModes `divMod` 10
      (argModes'', arg2Mode) = argModes' `divMod` 10
      (_, arg3Mode) = argModes'' `divMod` 10
      (headInput:input') = input
      arg1 = let i = ip + 1 in if arg1Mode == 0 then program V.! i else i
      arg1Val = program V.! arg1
      arg2 = let i = ip + 2 in if arg2Mode == 0 then program V.! i else i
      arg2Val = program V.! arg2
      arg3 = let i = ip + 3 in if arg3Mode == 0 then program V.! i else i
  in case opcode of
      1 -> doRunProgram state { program = (program V.// [(arg3, (arg1Val + arg2Val))]), ip = ip + 4 }
      2 -> doRunProgram state { program = (program V.// [(arg3, (arg1Val * arg2Val))]), ip = ip + 4 }
      3 -> if input == []
             then state { mode = WaitingForInput }
             else doRunProgram state { input = input', program = (program V.// [(arg1, headInput)]), ip = ip + 2 }
      4 -> doRunProgram state { output = arg1Val:output, ip = ip + 2 }
      5 -> doRunProgram state { ip = if arg1Val /= 0 then arg2Val else ip + 3 }
      6 -> doRunProgram state { ip = if arg1Val == 0 then arg2Val else ip + 3 }
      7 -> doRunProgram state { program = (program V.// [(arg3, if arg1Val < arg2Val then 1 else 0)]), ip = ip + 4 }
      8 -> doRunProgram state { program = (program V.// [(arg3, if arg1Val == arg2Val then 1 else 0)]), ip = ip + 4 }
      99 -> state { mode = Halted }
      x -> error ("unknown opcode " ++ show x)
