module Solvers (getSolver) where

import qualified Data.Vector as V
import Day01
import Day02
import Day03
import Day04
import Day05

solvers :: V.Vector (IO ())
solvers = V.fromList [Day01.solve, Day02.solve, Day03.solve, Day04.solve, Day05.solve]

getSolver :: Int -> Maybe (IO ())
getSolver day = solvers V.!? (day - 1)