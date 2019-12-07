{-# LANGUAGE RecordWildCards #-}

module Day05 (solve) where

import qualified Data.Vector.Unboxed as V
import Intcode (runProgram)
import Parsing (parseCommaSeparated, parseInt, resultOrError)

solve = do
  program <- V.fromList <$>
             resultOrError <$> parseCommaSeparated parseInt <$>
             readFile "../input/day05.txt"
  let output1 = runProgram program [1]
  putStrLn "Part 1:"
  print output1
  let output2 = runProgram program [5]
  putStrLn "Part 2:"
  print output2
