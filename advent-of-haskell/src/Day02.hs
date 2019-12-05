{-# LANGUAGE OverloadedStrings #-}

module Day02 (solve) where

import qualified Data.Vector.Unboxed as V
import Intcode (runSimple)
import Parsing (parseCommaSeparated, parseInt, resultOrError)

findInput _ [] _ = error "no input found"
findInput program ((a, b):inputs) desired =
  let finalState = runSimple (program V.// [(1, a), (2, b)])
  in if finalState V.! 0 == desired then (a, b) else findInput program inputs desired

solve :: IO ()
solve = do
  program <- V.fromList <$>
             resultOrError <$> parseCommaSeparated parseInt <$>
             readFile "../input/day02.txt"
  let programPart1 = program V.// [(1, 12), (2, 2)]
      finalState = runSimple programPart1
  putStrLn "Part 1:"
  print $ finalState V.! 0

  let inputs = [(x, y) | x <- [0..99], y <-[0..99]]
  let (noun, verb) = findInput program inputs 19690720
  putStrLn "Part 2:"
  print $ noun * 100 + verb
