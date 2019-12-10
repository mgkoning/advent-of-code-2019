module Day10 (solve, test) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (sortBy, maximumBy, reverse)
import Data.Ord (comparing)

type Coord = (Int, Int)
type Step = (Int, Int)
type Coords = [(Int, Int)]

readGrid :: [Char] -> HashMap Coord Char
readGrid s = 
  let gridLines = lines s -- [[Char]]
      enumeratedPositions = map (zip [0..]) gridLines -- [[(Int, Char)]]
      enumeratedGridLines = zip [0..] enumeratedPositions -- [(Int, [(Int, Char)])]
      coords = concat $ map (\(y, ps) -> map (\(x, c) -> ((x, y), c)) ps) enumeratedGridLines
  in M.filter (=='#') $ M.fromList coords

visibleAsteroids :: HashMap Coord Char -> [(Coord, Coords)]
visibleAsteroids m =
  let allAsteroids = M.keys m
      getVisible a = (a, filter (isVisible a) allAsteroids)
      isVisible a b = if a == b then False else nothingBetween a b
      nothingBetween a b = ([] ==) $ filter (`M.member` m) $ coordsBetween a b
  in map getVisible allAsteroids

coordsBetween :: Coord -> Coord -> Coords
coordsBetween from@(fromX, fromY) to@(toX, toY) =
   let step = (div diffX stepGcd, div diffY stepGcd)
       diffX = toX - fromX
       diffY = toY - fromY
       stepGcd = gcd diffX diffY
   in stepCoords from to step

stepCoords :: Coord -> Coord -> Step -> Coords
stepCoords from to step = takeWhile (/= to) $ drop 1 $ iterate (add step) from
  where add (x, y) (a, b) = (x + a, y + b)

angle :: Coord -> Double
angle (x, y) = let a = (atan2 (fromIntegral (-y)) (fromIntegral x)) + (3/2) * pi in if a > (2 * pi) then a - (2 * pi) else a

diff (x, y) (a, b) = (x - a, y - b)

solve = do
  grid <- readGrid <$> readFile "../input/day10.txt"
  putStrLn "Part 1:"
  let (station, asteroids) = maximumBy (comparing $ length . snd) $ visibleAsteroids grid
  print $ length asteroids
  putStrLn "Part 2:"
  let order = reverse $ sortBy (comparing snd) $ map (\c -> (c, angle $ diff c station)) asteroids
  print $ (\(a, b) -> a*100 + b) $ fst $ head $ drop 199 order

test = do
  let grid = readGrid testInput
  putStrLn "Part 1:"
  let (station, asteroids) = maximumBy (comparing $ length . snd) $ visibleAsteroids grid
  print $ length asteroids
  putStrLn "Part 2:"
  let order = reverse $ sortBy (comparing snd) $ map (\c -> (c, angle $ diff c station)) asteroids
  print station
  print order

testInput = ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##"
testInput2 = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"