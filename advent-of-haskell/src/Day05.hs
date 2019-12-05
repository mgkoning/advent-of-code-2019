{-# LANGUAGE RecordWildCards #-}

module Day05 (solve) where

import qualified Data.Vector.Unboxed as V
import Intcode (runProgram, State(..))
import Parsing (parseCommaSeparated, parseInt, resultOrError)

solve = do
  inputProgram <- V.fromList <$>
                  resultOrError <$> parseCommaSeparated parseInt <$>
                  readFile "../input/day05.txt"
  let State{..} = runProgram (State [1] inputProgram []) 0
  putStrLn "Part 1:"
  print output