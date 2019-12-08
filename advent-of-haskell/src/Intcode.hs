{-# LANGUAGE RecordWildCards #-}

module Intcode (
  readProgram,
  runSimple,
  Program, Input, Output, runProgram,
) where

import qualified Data.Vector.Unboxed as V
import Parsing (parseCommaSeparated, parseInt, resultOrError)

type Program = V.Vector Int
type Input = [Int]
type Output = [Int]

data State = State { input :: Input, program :: Program, ip :: Int }

readProgram :: String -> Program
readProgram = V.fromList . resultOrError . (parseCommaSeparated parseInt)

runSimple :: Program -> Program
runSimple p = program $ fst $ last $ doRunProgram (State [] p 0)

runProgram :: Program -> Input -> Output
runProgram program input = foldr (++) [] $ map snd $ doRunProgram (State input program 0)

doRunProgram :: State -> [(State, Output)]
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
      output = if opcode == 4 then [arg1Val] else []
      newState = 
        case opcode of
          1 -> state { program = (program V.// [(arg3, (arg1Val + arg2Val))]), ip = ip + 4 }
          2 -> state { program = (program V.// [(arg3, (arg1Val * arg2Val))]), ip = ip + 4 }
          3 -> state { input = input', program = (program V.// [(arg1, headInput)]), ip = ip + 2 }
          4 -> state { ip = ip + 2 }
          5 -> state { ip = if arg1Val /= 0 then arg2Val else ip + 3 }
          6 -> state { ip = if arg1Val == 0 then arg2Val else ip + 3 }
          7 -> state { program = (program V.// [(arg3, if arg1Val < arg2Val then 1 else 0)]), ip = ip + 4 }
          8 -> state { program = (program V.// [(arg3, if arg1Val == arg2Val then 1 else 0)]), ip = ip + 4 }
          x -> error ("unknown opcode " ++ show x)
  in if opcode == 99 then [] else (newState, output):(doRunProgram newState)
     
