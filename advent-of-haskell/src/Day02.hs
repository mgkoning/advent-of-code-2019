module Day02 (solve) where

import qualified Data.Vector.Unboxed as V
import Intcode (readProgram, runSimple, memSet, memGet)

findInput _ [] _ = error "no input found"
findInput program ((a, b):inputs) desired =
  let finalState = runSimple (memSet (memSet program 1 a) 2 b)
  in if memGet finalState 0 == desired then (a, b) else findInput program inputs desired

solve :: IO ()
solve = do
  program <- readProgram <$> readFile "../input/day02.txt"
  let programPart1 = memSet (memSet program 1 12) 2 2
      finalState = runSimple programPart1
  putStrLn "Part 1:"
  print $ memGet finalState 0

  let inputs = [(x, y) | x <- [0..99], y <-[0..99]]
  let (noun, verb) = findInput program inputs 19690720
  putStrLn "Part 2:"
  print $ noun * 100 + verb
