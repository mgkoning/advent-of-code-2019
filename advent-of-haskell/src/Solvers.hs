module Solvers (getSolver) where

import qualified Data.Vector as V
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20

solvers :: V.Vector (IO ())
solvers = V.fromList [
  Day01.solve, Day02.solve, Day03.solve, Day04.solve, Day05.solve,
  Day06.solve, Day07.solve, Day08.solve, Day09.solve, Day10.solve,
  Day11.solve, Day12.solve, Day13.solve, Day14.solve, Day15.solve,
  Day16.solve, Day17.solve, Day18.solve, Day19.solve, Day20.solve]

getSolver :: Int -> Maybe (IO ())
getSolver day = solvers V.!? (day - 1)