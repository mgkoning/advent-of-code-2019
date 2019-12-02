module Day01
  ( solve
  ) where

import Parsing (parseLines, parseInt, resultOrError)

fuelPart1 :: Int -> Int
fuelPart1 = (subtract 2) . (`div` 3)

fuelPart2 :: Int -> Int
fuelPart2 = fuelPart2' 0
  where
    fuelPart2' soFar next =
      let moreFuel = fuelPart1 next
          soFar' = soFar + moreFuel
      in if moreFuel <= 0 then soFar else soFar' `seq` fuelPart2' soFar' moreFuel

solve :: IO ()
solve = do
  modules <- resultOrError <$> parseLines parseInt <$> readFile "input/day01.txt"
  let part1 = map fuelPart1 modules
  putStrLn "Part 1:"
  print $ sum part1

  let part2 = map fuelPart2 modules
  putStrLn "Part 2:"
  print $ sum part2