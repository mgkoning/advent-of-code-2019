{-# LANGUAGE RecordWildCards #-}

module Intcode (
  readProgram, runSimple, memSet, memGet,
  Program, Input, Output, runProgram,
  doRunProgram, State(..)
) where

import qualified Data.Vector.Unboxed as V
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Parsing (parseCommaSeparated, parseInt, resultOrError)

type Program = IntMap Int
type Input = [Int]
type Output = [Int]

data State = State { input :: Input, program :: Program, ip :: Int, relBase :: Int } deriving (Show)

readProgram :: String -> Program
readProgram = M.fromList . zip [0..] . resultOrError . (parseCommaSeparated parseInt)

runSimple :: Program -> Program
runSimple p = program $ fst $ last $ doRunProgram (State [] p 0 0)

runProgram :: Program -> Input -> Output
runProgram program input = foldr (++) [] $ map snd $ doRunProgram (State input program 0 0)

doRunProgram :: State -> [(State, Output)]
doRunProgram state@(State {..}) =
  let current = program M.! ip
      (argModes, opcode) = current `divMod` 100
      (argModes', arg1Mode) = argModes `divMod` 10
      (argModes'', arg2Mode) = argModes' `divMod` 10
      (_, arg3Mode) = argModes'' `divMod` 10
      (headInput:input') = input
      paramMemPos mode pos =
         let i = ip + pos
         in case mode of
              0 -> memGet program i
              1 -> i
              2 -> relBase + memGet program i
      arg1 = paramMemPos arg1Mode 1
      arg1Val = memGet program arg1
      arg2 = paramMemPos arg2Mode 2
      arg2Val = memGet program arg2
      arg3 = paramMemPos arg3Mode 3
      output = if opcode == 4 then [arg1Val] else []
      newState = 
        case opcode of
          1 -> state { program = memSet program arg3 (arg1Val + arg2Val), ip = ip + 4 }
          2 -> state { program = memSet program arg3 (arg1Val * arg2Val), ip = ip + 4 }
          3 -> state { input = input', program = memSet program arg1 headInput, ip = ip + 2 }
          4 -> state { ip = ip + 2 }
          5 -> state { ip = if arg1Val /= 0 then arg2Val else ip + 3 }
          6 -> state { ip = if arg1Val == 0 then arg2Val else ip + 3 }
          7 -> state { program = memSet program arg3 (if arg1Val < arg2Val then 1 else 0), ip = ip + 4 }
          8 -> state { program = memSet program arg3 (if arg1Val == arg2Val then 1 else 0), ip = ip + 4 }
          9 -> state { relBase = relBase + arg1Val, ip = ip + 2 }
          x -> error ("unknown opcode " ++ show x)
  in if opcode == 99 then [] else (newState, output):(doRunProgram newState)
     
memGet prog loc = M.findWithDefault 0 loc prog
memSet prog loc val = M.insert loc val prog