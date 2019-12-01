module Day1
  ( solve
  ) where

readInt :: String -> Int
readInt = read

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
  input <- readFile "input/day01.txt"
  let modules = map readInt (lines input)
  let part1 = map fuelPart1 modules
  putStrLn "Part 1:"
  print $ sum part1

  let part2 = map fuelPart2 modules
  putStrLn "Part 2:"
  print $ sum part2