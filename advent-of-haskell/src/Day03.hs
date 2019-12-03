module Day03 (solve) where

import Text.Parsec (letter, sepEndBy, string)
import Text.Parsec.String (Parser)
import Parsing (parseLines, parseInt, resultOrError)
import Prelude hiding (Left, Right)
import Data.HashSet (fromList, toList, intersection)
import Data.List (sort)

data Direction = Left | Right | Up | Down deriving (Show, Eq)

type Turn = (Direction, Int)
type Path = [Turn]

type Coord = (Int, Int)

add (x, y) (a, b) = (x + a, y + b)

parseDirection :: Parser Direction
parseDirection = mkDirection <$> letter
  where mkDirection d = case d of
                          'R' -> Right
                          'L' -> Left
                          'U' -> Up
                          'D' -> Down

parsePath :: Parser Path
parsePath = mkStep `sepEndBy` (string ",")
  where mkStep = (,) <$> parseDirection <*> parseInt

parseWires = parseLines parsePath

coordChange direction = case direction of
                          Right ->  (1, 0)
                          Left  -> (-1, 0)
                          Up    ->  (0, 1)
                          Down  -> (0, -1)

getSteps (direction, count) = replicate count $ coordChange direction

getCoords from turn = foldl addStep (from, []) (getSteps turn)
  where addStep (from, coords) step = let next = add from step in (next, next:coords)

getFullPath :: Path -> (Coord, [Coord])
getFullPath turns = foldl addTurn ((0, 0), []) turns
  where addTurn (from, steps) turn =
          let (endCoord, steps') = getCoords from turn
          in (endCoord, steps ++ steps')

manhattan (x, y) = abs x + abs y

solve = do
  (wire1:wire2:_) <- resultOrError <$> parseWires <$> readFile "input/day03.txt"
  putStrLn "Part 1:"
  let path1 = fromList $ snd $ getFullPath wire1
      path2 = fromList $ snd $ getFullPath wire2
      both = intersection path1 path2
      distances = map manhattan $ toList both
  print $ head $ sort $ distances