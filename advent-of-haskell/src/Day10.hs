module Day10 (solve, coordsBetween) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (sort, maximumBy)

readGrid :: [Char] -> HashMap (Int, Int) Char
readGrid s = 
  let gridLines = lines s -- [[Char]]
      enumeratedPositions = map (zip [0..]) gridLines -- [[(Int, Char)]]
      enumeratedGridLines = zip [0..] enumeratedPositions -- [(Int, [(Int, Char)])]
      coords = concat $ map (\(y, ps) -> map (\(x, c) -> ((x, y), c)) ps) enumeratedGridLines
  in M.filter (=='#') $ M.fromList coords

visibleAsteroids :: HashMap (Int, Int) Char -> [Int]
visibleAsteroids m =
  let allAsteroids = M.keys m
      countVisible a = length $ filter (isVisible a) allAsteroids
      isVisible a b = if a == b then False else nothingBetween a b
      nothingBetween a b = ([] ==) $ filter (`M.member` m) $ coordsBetween a b
  in map countVisible allAsteroids

coordsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
coordsBetween from@(fromX, fromY) to@(toX, toY) =
   let step = (div diffX stepGcd, div diffY stepGcd)
       diffX = toX - fromX
       diffY = toY - fromY
       stepGcd = gcd diffX diffY
   in stepCoords from to step

stepCoords :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
stepCoords from to step = takeWhile (/= to) $ drop 1 $ iterate (add step) from
  where add (x, y) (a, b) = (x + a, y + b)

solve = do
  grid <- readGrid <$> readFile "../input/day10.txt"
  putStrLn "Part 1:"
  let winner = maximum $ visibleAsteroids grid
  print winner
