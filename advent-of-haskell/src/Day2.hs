{-# LANGUAGE OverloadedStrings #-}

module Day2 (solve) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Text as T
import Debug.Trace (traceShowId)

readInt :: String -> Int
readInt = read

runProgram state pointer =
  let current = state V.! pointer
      lhs = state V.! (state V.! (pointer + 1))
      rhs = state V.! (state V.! (pointer + 2))
      outputPos = state V.! (pointer + 3)
  in case current of
      1 -> runProgram (state V.// [(outputPos, (lhs + rhs))]) (pointer + 4)
      2 -> runProgram (state V.// [(outputPos, (lhs * rhs))]) (pointer + 4)
      99 -> state
      x -> error ("unknown opcode " ++ (show x))

findInput _ [] _ = error "no input found"
findInput program ((a, b):inputs) desired =
  let finalState = runProgram (program V.// [(1, a), (2, b)]) 0
  in if finalState V.! 0 == desired then (a, b) else findInput program inputs desired

solve :: IO ()
solve = do
  program <- V.fromList <$>
    ((map (readInt . T.unpack)) . (T.split (\c -> c == ',')) . (T.pack)) <$>
    readFile "input/day02.txt"

  let programPart1 = program V.// [(1, 12), (2, 2)]
  let finalState = runProgram programPart1 0
  putStrLn "Part 1:"
  print $ finalState V.! 0

  let inputs = [(x, y) | x <- [0..99], y <-[0..99]]
  let (noun, verb) = findInput program inputs 19690720
  putStrLn "Part 2:"
  print $ noun * 100 + verb
